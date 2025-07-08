from metaflow import FlowSpec, step, Parameter, current
import pandas as pd
import numpy as np
import mlflow
import mlflow.sklearn
import joblib
import os
import json
from datetime import datetime
from typing import Dict, Any, Tuple
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report, accuracy_score, f1_score
from sentence_transformers import SentenceTransformer
from sklearn.cluster import HDBSCAN
from lib.config.settings import settings
from lib.utils.aws_utils import load_data_from_s3
from lib.utils.mlflow_utils import setup_mlflow, log_model_metadata

class ModelTrainingFlow(FlowSpec):
    """Complete model training flow with MLflow tracking and model comparison"""

    # Parameters
    s3_bucket = Parameter('s3_bucket', default=settings.S3_BUCKET)
    s3_key = Parameter('s3_key', default=settings.S3_KEY)
    test_size = Parameter('test_size', default=settings.TEST_SIZE)
    random_state = Parameter('random_state', default=settings.RANDOM_STATE)
    min_cluster_size = Parameter('min_cluster_size', default=settings.MIN_CLUSTER_SIZE)
    min_samples = Parameter('min_samples', default=settings.MIN_SAMPLES)

    @step
    def start(self):
        """Initialize the flow"""
        print(f"Starting model training flow with run_id: {current.run_id}")
        self.next(self.load_data)

    @step
    def load_data(self):
        """Load data from S3 and perform initial preprocessing"""
        print("Loading data from S3...")

        # Load data from S3
        data = load_data_from_s3(self.s3_bucket, self.s3_key)

        if not data:
            print("No data from S3, creating sample data...")
            data = self._create_sample_data()

        # Convert to DataFrame
        df = pd.DataFrame(data)
        df['doneThing'] = df['doneThing'].fillna('')

        # Basic preprocessing
        df['doneThing_clean'] = df['doneThing'].str.lower().str.strip()
        df['text_length'] = df['doneThing_clean'].str.len()

        # Filter out empty or very short entries
        df = df[df['text_length'] >= 10]
        df = df.drop_duplicates(subset=['doneThing_clean'])

        self.raw_data = df
        print(f"Loaded {len(self.raw_data)} items for processing")
        self.next(self.discover_topics)

    @step
    def discover_topics(self):
        """Discover topics using clustering"""
        print("Discovering topics using clustering...")

        # Generate embeddings
        model = SentenceTransformer('all-MiniLM-L6-v2')
        texts = self.raw_data['doneThing_clean'].tolist()
        embeddings = model.encode(texts, show_progress_bar=True)

        # Cluster texts
        clustering = HDBSCAN(
            min_cluster_size=self.min_cluster_size,
            min_samples=self.min_samples,
            metric='euclidean'
        )

        cluster_labels = clustering.fit_predict(embeddings)
        self.raw_data['cluster_id'] = cluster_labels

        # Filter out noise (cluster_id = -1)
        clustered_data = self.raw_data[self.raw_data['cluster_id'] >= 0]

        # Analyze clusters and create topic mapping
        topic_mapping = {}
        for cluster_id in clustered_data['cluster_id'].unique():
            cluster_texts = clustered_data[
                clustered_data['cluster_id'] == cluster_id
            ]['doneThing_clean'].tolist()

            # Extract common words and suggest topic
            common_words = self._extract_common_words(cluster_texts)
            suggested_topic = self._suggest_topic(common_words)
            topic_mapping[cluster_id] = suggested_topic

        # Add topic labels
        clustered_data['topic'] = clustered_data['cluster_id'].map(topic_mapping)

        # Filter out items without topics
        self.training_data = clustered_data[clustered_data['topic'].notna()]

        print(f"Discovered {len(topic_mapping)} topics")
        for cluster_id, topic in topic_mapping.items():
            count = len(clustered_data[clustered_data['cluster_id'] == cluster_id])
            print(f"  Cluster {cluster_id}: {topic} ({count} items)")

        self.next(self.split_data)

    @step
    def split_data(self):
        """Split data into train/test sets"""
        print("Splitting data into train/test sets...")

        X = self.training_data['doneThing_clean']
        y = self.training_data['topic']

        X_train, X_test, y_train, y_test = train_test_split(
            X, y, test_size=self.test_size, random_state=self.random_state,
            stratify=y
        )

        self.X_train = X_train
        self.X_test = X_test
        self.y_train = y_train
        self.y_test = y_test

        print(f"Training set: {len(self.X_train)} samples")
        print(f"Test set: {len(self.X_test)} samples")
        print(f"Classes: {sorted(y.unique())}")

        self.next(self.train_model)

    @step
    def train_model(self):
        """Train the classification model"""
        print("Training classification model...")

        # Feature extraction
        vectorizer = TfidfVectorizer(
            max_features=5000,
            ngram_range=(1, 2),
            stop_words='english'
        )

        X_train_vectors = vectorizer.fit_transform(self.X_train)
        X_test_vectors = vectorizer.transform(self.X_test)

        # Train model
        model = RandomForestClassifier(
            n_estimators=100,
            random_state=self.random_state,
            n_jobs=-1
        )

        model.fit(X_train_vectors, self.y_train)

        # Make predictions
        y_pred = model.predict(X_test_vectors)
        y_pred_proba = model.predict_proba(X_test_vectors)

        # Calculate metrics
        accuracy = accuracy_score(self.y_test, y_pred)
        f1_macro = f1_score(self.y_test, y_pred, average='macro')
        f1_weighted = f1_score(self.y_test, y_pred, average='weighted')

        report = classification_report(self.y_test, y_pred, output_dict=True)

        self.model = model
        self.vectorizer = vectorizer
        self.metrics = {
            'accuracy': accuracy,
            'f1_macro': f1_macro,
            'f1_weighted': f1_weighted,
            'classification_report': report
        }
        self.y_pred = y_pred
        self.y_pred_proba = y_pred_proba

        print(f"Model trained with accuracy: {accuracy:.4f}")
        print(f"F1 Macro: {f1_macro:.4f}")
        print(f"F1 Weighted: {f1_weighted:.4f}")

        self.next(self.evaluate_model)

    @step
    def evaluate_model(self):
        """Evaluate model and log to MLflow"""
        print("Evaluating model and logging to MLflow...")

        setup_mlflow()

        with mlflow.start_run():
            # Log run info
            mlflow.set_tag("flow_name", "ModelTrainingFlow")
            mlflow.set_tag("run_id", current.run_id)

            # Log parameters
            mlflow.log_param("s3_bucket", self.s3_bucket)
            mlflow.log_param("s3_key", self.s3_key)
            mlflow.log_param("test_size", self.test_size)
            mlflow.log_param("random_state", self.random_state)
            mlflow.log_param("min_cluster_size", self.min_cluster_size)
            mlflow.log_param("min_samples", self.min_samples)
            mlflow.log_param("training_samples", len(self.X_train))
            mlflow.log_param("test_samples", len(self.X_test))
            mlflow.log_param("n_classes", len(self.y_test.unique()))

            # Log metrics
            for metric_name, metric_value in self.metrics.items():
                if metric_name != 'classification_report':
                    mlflow.log_metric(metric_name, metric_value)

            # Log detailed classification report
            for class_name, metrics in self.metrics['classification_report'].items():
                if isinstance(metrics, dict):
                    for metric_name, metric_value in metrics.items():
                        if isinstance(metric_value, (int, float)):
                            mlflow.log_metric(f"{class_name}_{metric_name}", metric_value)

            # Log model
            mlflow.sklearn.log_model(self.model, "model")
            mlflow.sklearn.log_model(self.vectorizer, "vectorizer")

            # Log model metadata
            model_metadata = {
                "model_type": "RandomForestClassifier",
                "vectorizer_type": "TfidfVectorizer",
                "classes": sorted(self.y_test.unique().tolist()),
                "training_samples": len(self.X_train),
                "test_samples": len(self.X_test),
                "run_id": current.run_id,
                "timestamp": datetime.now().isoformat()
            }
            log_model_metadata(model_metadata)

            # Store run info for comparison
            self.run_id = mlflow.active_run().info.run_id
            self.model_uri = mlflow.get_artifact_uri("model")
            self.vectorizer_uri = mlflow.get_artifact_uri("vectorizer")

        self.next(self.compare_models)

    @step
    def compare_models(self):
        """Compare with current best model and update if better"""
        print("Comparing with current best model...")

        # Get current best model info
        client = mlflow.tracking.MlflowClient()

        try:
            # Get the current best model from the model registry
            current_best = client.get_latest_versions("topic-classifier", stages=["Production"])
            if current_best:
                current_best_run = current_best[0]
                current_best_metrics = client.get_run(current_best_run.run_id).data.metrics
                current_best_accuracy = current_best_metrics.get('accuracy', 0.0)
                current_best_f1 = current_best_metrics.get('f1_macro', 0.0)
            else:
                current_best_accuracy = 0.0
                current_best_f1 = 0.0
        except Exception as e:
            print(f"No current best model found: {e}")
            current_best_accuracy = 0.0
            current_best_f1 = 0.0

        # Compare metrics
        new_accuracy = self.metrics['accuracy']
        new_f1 = self.metrics['f1_macro']

        is_better = False
        improvement_reason = ""

        # Check if new model is better (using F1 macro as primary metric)
        if new_f1 > current_best_f1:
            is_better = True
            improvement_reason = f"F1 macro improved: {current_best_f1:.4f} -> {new_f1:.4f}"
        elif new_f1 == current_best_f1 and new_accuracy > current_best_accuracy:
            is_better = True
            improvement_reason = f"F1 macro equal, accuracy improved: {current_best_accuracy:.4f} -> {new_accuracy:.4f}"

        self.is_better = is_better
        self.improvement_reason = improvement_reason
        self.current_best_metrics = {
            'accuracy': current_best_accuracy,
            'f1_macro': current_best_f1
        }
        self.new_model_metrics = {
            'accuracy': new_accuracy,
            'f1_macro': new_f1
        }

        print(f"Current best - Accuracy: {current_best_accuracy:.4f}, F1: {current_best_f1:.4f}")
        print(f"New model - Accuracy: {new_accuracy:.4f}, F1: {new_f1:.4f}")
        print(f"Is better: {is_better}")
        if is_better:
            print(f"Improvement: {improvement_reason}")

        self.next(self.update_best_model)

    @step
    def update_best_model(self):
        """Update the best model if the new model is better"""
        print("Updating best model...")

        client = mlflow.tracking.MlflowClient()

        if self.is_better:
            try:
                # Register the new model
                model_name = "topic-classifier"
                model_version = client.create_model_version(
                    name=model_name,
                    source=self.model_uri,
                    run_id=self.run_id
                )

                # Transition to Production
                client.transition_model_version_stage(
                    name=model_name,
                    version=model_version.version,
                    stage="Production"
                )

                # Archive previous production models
                for version in client.search_model_versions(f"name='{model_name}'"):
                    if version.current_stage == "Production" and version.version != model_version.version:
                        client.transition_model_version_stage(
                            name=model_name,
                            version=version.version,
                            stage="Archived"
                        )

                print(f"New model promoted to production! Version: {model_version.version}")

                # Save model artifacts locally for batch prediction
                os.makedirs('data/models', exist_ok=True)
                joblib.dump(self.model, 'data/models/topic_classifier.pkl')
                joblib.dump(self.vectorizer, 'data/models/vectorizer.pkl')

                model_metadata = {
                    "version": model_version.version,
                    "run_id": self.run_id,
                    "accuracy": self.new_model_metrics['accuracy'],
                    "f1_macro": self.new_model_metrics['f1_macro'],
                    "classes": sorted(self.y_test.unique().tolist()),
                    "training_samples": len(self.X_train),
                    "test_samples": len(self.X_test),
                    "timestamp": datetime.now().isoformat(),
                    "improvement_reason": self.improvement_reason
                }

                with open('data/models/model_metadata.json', 'w') as f:
                    json.dump(model_metadata, f, indent=2)

            except Exception as e:
                print(f"Error updating best model: {e}")
        else:
            print("New model is not better than current best. Keeping current model.")

        self.next(self.end)

    @step
    def end(self):
        """Flow completion"""
        print("Model training flow completed!")
        print(f"Run ID: {current.run_id}")
        if self.is_better:
            print("✅ New model promoted to production!")
        else:
            print("❌ New model not promoted (not better than current best)")

    def _extract_common_words(self, texts, top_n=10):
        """Extract most common words from cluster texts"""
        from collections import Counter
        import re

        all_words = []
        for text in texts:
            words = re.findall(r'\b\w+\b', text.lower())
            all_words.extend(words)

        word_counts = Counter(all_words)
        return [word for word, count in word_counts.most_common(top_n)]

    def _suggest_topic(self, common_words):
        """Suggest topic name based on common words"""
        topic_mapping = {
            'meeting': 'meetings',
            'call': 'calls',
            'email': 'communication',
            'code': 'development',
            'bug': 'development',
            'test': 'development',
            'deploy': 'deployment',
            'shop': 'shopping',
            'buy': 'shopping',
            'purchase': 'shopping',
            'read': 'reading',
            'book': 'reading',
            'exercise': 'fitness',
            'workout': 'fitness',
            'run': 'fitness',
            'cook': 'cooking',
            'food': 'cooking',
            'meal': 'cooking',
            'learn': 'learning',
            'study': 'learning',
            'research': 'learning',
            'write': 'writing',
            'document': 'writing',
            'plan': 'planning',
            'design': 'design',
            'review': 'review',
            'fix': 'maintenance',
            'clean': 'maintenance',
            'organize': 'organization'
        }

        for word in common_words:
            if word in topic_mapping:
                return topic_mapping[word]

        return 'general'

    def _create_sample_data(self):
        """Create sample data for testing"""
        sample_data = [
            {"doneThing": "Completed user authentication module", "date": "2024-01-01"},
            {"doneThing": "Fixed bug in payment processing", "date": "2024-01-02"},
            {"doneThing": "Had team meeting to discuss project timeline", "date": "2024-01-03"},
            {"doneThing": "Wrote documentation for API endpoints", "date": "2024-01-04"},
            {"doneThing": "Deployed new version to production", "date": "2024-01-05"},
            {"doneThing": "Bought groceries for the week", "date": "2024-01-06"},
            {"doneThing": "Read chapter 5 of the machine learning book", "date": "2024-01-07"},
            {"doneThing": "Went for a 5km run", "date": "2024-01-08"},
            {"doneThing": "Cooked dinner for the family", "date": "2024-01-09"},
            {"doneThing": "Reviewed pull request from colleague", "date": "2024-01-10"},
            {"doneThing": "Organized the kitchen", "date": "2024-01-11"},
            {"doneThing": "Watched a movie with friends", "date": "2024-01-12"},
            {"doneThing": "Went to the zoo", "date": "2024-01-24"},
            {"doneThing": "Implemented CI/CD pipeline with GitHub Actions", "date": "2024-01-13"},
            {"doneThing": "Set up monitoring with Prometheus and Grafana", "date": "2024-01-14"},
            {"doneThing": "Optimized database queries for better performance", "date": "2024-01-15"},
            {"doneThing": "Built data pipeline using Apache Airflow", "date": "2024-01-16"},
            {"doneThing": "Trained machine learning model for sentiment analysis", "date": "2024-01-17"},
            {"doneThing": "Deployed model to production using Docker containers", "date": "2024-01-18"},
            {"doneThing": "Implemented feature store for ML model serving", "date": "2024-01-19"},
            {"doneThing": "Set up data lake architecture on AWS", "date": "2024-01-20"},
            {"doneThing": "Built real-time streaming pipeline with Kafka", "date": "2024-01-21"},
            {"doneThing": "Implemented A/B testing framework", "date": "2024-01-22"},
            {"doneThing": "Created automated data quality checks", "date": "2024-01-23"},
            {"doneThing": "Built recommendation system using collaborative filtering", "date": "2024-01-25"},
            {"doneThing": "Implemented microservices architecture", "date": "2024-01-26"},
            {"doneThing": "Set up Kubernetes cluster for container orchestration", "date": "2024-01-27"},
            {"doneThing": "Built ETL pipeline for data warehouse", "date": "2024-01-28"},
            {"doneThing": "Implemented GraphQL API for frontend integration", "date": "2024-01-29"},
            {"doneThing": "Created automated backup system for databases", "date": "2024-01-30"},
            {"doneThing": "Built real-time dashboard with React and D3.js", "date": "2024-01-31"},
            {"doneThing": "Implemented OAuth2 authentication system", "date": "2024-02-01"},
            {"doneThing": "Set up automated testing with Jest and Cypress", "date": "2024-02-02"},
            {"doneThing": "Built data preprocessing pipeline for ML models", "date": "2024-02-03"},
            {"doneThing": "Implemented caching strategy with Redis", "date": "2024-02-04"},
            {"doneThing": "Created automated deployment pipeline", "date": "2024-02-05"},
            {"doneThing": "Built API gateway with rate limiting", "date": "2024-02-06"},
            {"doneThing": "Implemented event-driven architecture", "date": "2024-02-07"},
            {"doneThing": "Set up data versioning with DVC", "date": "2024-02-08"},
            {"doneThing": "Built automated model retraining pipeline", "date": "2024-02-09"},
            {"doneThing": "Implemented blue-green deployment strategy", "date": "2024-02-10"},
            {"doneThing": "Created automated security scanning pipeline", "date": "2024-02-11"},
            {"doneThing": "Built real-time analytics platform", "date": "2024-02-12"},
            {"doneThing": "Implemented feature flags for gradual rollouts", "date": "2024-02-13"},
            {"doneThing": "Set up automated performance testing", "date": "2024-02-14"},
            {"doneThing": "Built data lineage tracking system", "date": "2024-02-15"},
            {"doneThing": "Implemented automated data backup and recovery", "date": "2024-02-16"},
            {"doneThing": "Created automated documentation generator", "date": "2024-02-17"},
            {"doneThing": "Built automated code review system", "date": "2024-02-18"},
            {"doneThing": "Implemented automated dependency vulnerability scanning", "date": "2024-02-19"},
            {"doneThing": "Set up automated cost monitoring for cloud resources", "date": "2024-02-20"},
            {"doneThing": "Built automated data validation pipeline", "date": "2024-02-21"},
            {"doneThing": "Implemented automated model performance monitoring", "date": "2024-02-22"},
            {"doneThing": "Created automated data privacy compliance checks", "date": "2024-02-23"},
            {"doneThing": "Built automated data governance framework", "date": "2024-02-24"},
            {"doneThing": "Implemented automated data retention policies", "date": "2024-02-25"},
            {"doneThing": "Set up automated data encryption at rest and in transit", "date": "2024-02-26"},
            {"doneThing": "Built automated data anonymization pipeline", "date": "2024-02-27"},
            {"doneThing": "Implemented automated data quality scoring", "date": "2024-02-28"},
            {"doneThing": "Created automated data catalog with metadata management", "date": "2024-02-29"},
        ]
        return sample_data

if __name__ == '__main__':
    ModelTrainingFlow()
