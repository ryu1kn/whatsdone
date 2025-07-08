import mlflow
import os
import sys
from typing import Dict, Any, Optional
sys.path.append(os.path.dirname(os.path.dirname(__file__)))
from config.settings import settings

def setup_mlflow():
    """Setup MLflow tracking"""
    mlflow.set_tracking_uri(settings.MLFLOW_TRACKING_URI)
    mlflow.set_experiment(settings.MLFLOW_EXPERIMENT_NAME)

def get_best_model():
    """Get the best model from MLflow registry"""
    client = mlflow.tracking.MlflowClient()

    try:
        model_versions = client.get_latest_versions("topic-classifier", stages=["Production"])
        if model_versions:
            return model_versions[0]
    except Exception as e:
        print(f"Error getting best model: {e}")

    return None

def log_model_metadata(metadata: Dict[str, Any]):
    """Log model metadata to MLflow"""
    mlflow.log_dict(metadata, "model_metadata.json")

def log_prediction_stats(stats: Dict[str, Any]):
    """Log prediction statistics to MLflow"""
    mlflow.log_dict(stats, "prediction_stats.json")
