import os
from typing import Dict, Any

class Settings:
    # AWS Configuration
    AWS_REGION = os.getenv('AWS_DEFAULT_REGION', 'ap-southeast-2')
    DYNAMODB_TABLE = os.getenv('DYNAMODB_TABLE', 'whatsdone-dones-dev-ryuichi')

    # S3 Configuration
    S3_BUCKET = os.getenv('S3_BUCKET', 'whatsdone-data')
    S3_KEY = os.getenv('S3_KEY', 'training/done_items.csv')

    # MLflow Configuration
    MLFLOW_TRACKING_URI = os.getenv('MLFLOW_TRACKING_URI', 'http://localhost:5001')
    MLFLOW_EXPERIMENT_NAME = os.getenv('MLFLOW_EXPERIMENT_NAME', 'topic-classification')

    # Model Configuration
    MODEL_PATH = os.getenv('MODEL_PATH', 'data/models')
    DATA_PATH = os.getenv('DATA_PATH', 'data')

    # Pipeline Configuration
    MIN_CLUSTER_SIZE = int(os.getenv('MIN_CLUSTER_SIZE', '2'))
    MIN_SAMPLES = int(os.getenv('MIN_SAMPLES', '1'))
    TEST_SIZE = float(os.getenv('TEST_SIZE', '0.2'))
    RANDOM_STATE = int(os.getenv('RANDOM_STATE', '42'))
    BATCH_SIZE = int(os.getenv('BATCH_SIZE', '100'))

settings = Settings()
