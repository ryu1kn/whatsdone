from metaflow import FlowSpec, step, Parameter, current
import pandas as pd
import numpy as np
import mlflow
import joblib
import os
import json
from datetime import datetime
from typing import Dict, Any, List
from mlflow.tracking import MlflowClient
from lib.config.settings import settings
from lib.utils.aws_utils import scan_dynamodb_table, update_dynamodb_item
from lib.utils.mlflow_utils import setup_mlflow, get_best_model, log_prediction_stats

class BatchPredictionFlow(FlowSpec):
    """Batch prediction flow to update DynamoDB with topic classifications"""

    # Parameters
    table_name = Parameter('table_name', default=settings.DYNAMODB_TABLE)
    region = Parameter('region', default=settings.AWS_REGION)
    batch_size = Parameter('batch_size', default=settings.BATCH_SIZE)

    @step
    def start(self):
        """Initialize the flow"""
        print(f"Starting batch prediction flow with run_id: {current.run_id}")
        self.next(self.load_model)

    @step
    def load_model(self):
        """Load the best model from MLflow or local storage"""
        print("Loading best model...")

        client = mlflow.tracking.MlflowClient()

        try:
            # Try to get the production model from MLflow
            model_versions = client.get_latest_versions("topic-classifier", stages=["Production"])
            if model_versions:
                best_model = model_versions[0]
                print(f"Loading production model version {best_model.version}")

                # Load model and vectorizer from MLflow
                self.model = mlflow.sklearn.load_model(f"runs:/{best_model.run_id}/model")
                self.vectorizer = mlflow.sklearn.load_model(f"runs:/{best_model.run_id}/vectorizer")

                # Get model metadata
                run = client.get_run(best_model.run_id)
                self.model_metadata = {
                    "version": best_model.version,
                    "run_id": best_model.run_id,
                    "accuracy": run.data.metrics.get('accuracy', 0.0),
                    "f1_macro": run.data.metrics.get('f1_macro', 0.0),
                    "classes": run.data.params.get('classes', '[]'),
                    "timestamp": best_model.creation_timestamp
                }

            else:
                # Fallback to local model
                print("No production model found, loading local model...")
                self._load_local_model()

        except Exception as e:
            print(f"Error loading from MLflow: {e}")
            print("Falling back to local model...")
            self._load_local_model()

        print(f"Model loaded - Version: {self.model_metadata.get('version', 'local')}")
        print(f"Accuracy: {self.model_metadata.get('accuracy', 0.0):.4f}")
        print(f"F1 Macro: {self.model_metadata.get('f1_macro', 0.0):.4f}")

        self.next(self.load_production_data)

    @step
    def load_production_data(self):
        """Load all done items from DynamoDB"""
        print("Loading production data from DynamoDB...")

        # Load data from DynamoDB
        items = scan_dynamodb_table(self.table_name)

        # Convert to DataFrame
        df = pd.DataFrame(items)
        df['doneThing'] = df['doneThing'].fillna('')

        # Filter out empty doneThing entries
        df = df[df['doneThing'].str.len() > 0]

        # Filter out items that already have topics (optional)
        # df = df[df['topics'].isna() | (df['topics'].apply(lambda x: len(x) == 0))]

        self.production_data = df
        print(f"Loaded {len(self.production_data)} items for prediction")

        self.next(self.predict_topics)

    @step
    def predict_topics(self):
        """Predict topics for all production data"""
        print("Predicting topics...")

        # Preprocess text
        texts = self.production_data['doneThing'].str.lower().str.strip().tolist()

        # Vectorize
        X = self.vectorizer.transform(texts)

        # Predict
        predictions = self.model.predict(X)
        probabilities = self.model.predict_proba(X)
        confidences = np.max(probabilities, axis=1)

        # Add predictions to data
        self.production_data['predicted_topics'] = predictions
        self.production_data['confidence'] = confidences

        # Convert topics to list format for DynamoDB
        self.production_data['topics'] = self.production_data['predicted_topics'].apply(
            lambda x: [x] if x else []
        )

        # Calculate prediction statistics
        unique_topics = self.production_data['predicted_topics'].unique()
        topic_counts = self.production_data['predicted_topics'].value_counts()
        avg_confidence = self.production_data['confidence'].mean()

        self.prediction_stats = {
            'total_items': len(self.production_data),
            'unique_topics': len(unique_topics),
            'avg_confidence': avg_confidence,
            'topic_distribution': topic_counts.to_dict()
        }

        print(f"Predicted topics for {len(self.production_data)} items")
        print(f"Unique topics: {list(unique_topics)}")
        print(f"Average confidence: {avg_confidence:.4f}")
        print("Topic distribution:")
        for topic, count in topic_counts.items():
            print(f"  {topic}: {count} items")

        self.next(self.update_dynamodb)

    @step
    def update_dynamodb(self):
        """Update DynamoDB with predicted topics"""
        print("Updating DynamoDB with predicted topics...")

        updated_count = 0
        error_count = 0
        errors = []

        # Process in batches
        for i in range(0, len(self.production_data), self.batch_size):
            batch = self.production_data.iloc[i:i+self.batch_size]

            for _, row in batch.iterrows():
                try:
                    # Update item with predicted topics
                    success = update_dynamodb_item(
                        self.table_name,
                        row['id'],
                        row['topics'],
                        datetime.now().isoformat()
                    )
                    if success:
                        updated_count += 1
                    else:
                        error_count += 1
                        errors.append(f"Item {row['id']}: Update failed")
                except Exception as e:
                    error_count += 1
                    errors.append(f"Item {row['id']}: {str(e)}")
                    print(f"Error updating item {row['id']}: {e}")

        self.update_stats = {
            'updated_count': updated_count,
            'error_count': error_count,
            'errors': errors
        }

        print(f"Updated {updated_count} items")
        print(f"Errors: {error_count} items")

        self.next(self.log_results)

    @step
    def log_results(self):
        """Log results to MLflow"""
        print("Logging results to MLflow...")

        setup_mlflow()

        with mlflow.start_run():
            # Log run info
            mlflow.set_tag("flow_name", "BatchPredictionFlow")
            mlflow.set_tag("run_id", current.run_id)
            mlflow.set_tag("model_version", self.model_metadata.get('version', 'local'))

            # Log parameters
            mlflow.log_param("table_name", self.table_name)
            mlflow.log_param("region", self.region)
            mlflow.log_param("batch_size", self.batch_size)
            mlflow.log_param("total_items", self.prediction_stats['total_items'])

            # Log metrics
            mlflow.log_metric("updated_count", self.update_stats['updated_count'])
            mlflow.log_metric("error_count", self.update_stats['error_count'])
            mlflow.log_metric("success_rate",
                            self.update_stats['updated_count'] / self.prediction_stats['total_items'])
            mlflow.log_metric("avg_confidence", self.prediction_stats['avg_confidence'])
            mlflow.log_metric("unique_topics", self.prediction_stats['unique_topics'])

            # Log detailed statistics
            log_prediction_stats(self.prediction_stats)
            mlflow.log_dict(self.update_stats, "update_stats.json")
            mlflow.log_dict(self.model_metadata, "model_metadata.json")

        self.next(self.end)

    @step
    def end(self):
        """Flow completion"""
        print("Batch prediction flow completed!")
        print(f"Run ID: {current.run_id}")
        print(f"Model Version: {self.model_metadata.get('version', 'local')}")
        print(f"Updated {self.update_stats['updated_count']} items")
        print(f"Errors: {self.update_stats['error_count']} items")
        print(f"Success rate: {self.update_stats['updated_count'] / self.prediction_stats['total_items']:.2%}")

    def _load_local_model(self):
        """Load model from local storage"""
        model_path = 'data/models'

        if os.path.exists(f'{model_path}/topic_classifier.pkl'):
            self.model = joblib.load(f'{model_path}/topic_classifier.pkl')
            self.vectorizer = joblib.load(f'{model_path}/vectorizer.pkl')

            # Load metadata
            with open(f'{model_path}/model_metadata.json', 'r') as f:
                self.model_metadata = json.load(f)
        else:
            raise FileNotFoundError("No local model found. Please run training first.")

if __name__ == '__main__':
    BatchPredictionFlow()
