#!/usr/bin/env python3
"""
Simple test script to verify ML infrastructure setup
"""

import os
import sys

def test_imports():
    """Test that all required modules can be imported"""
    try:
        from src.config.settings import settings
        print("✅ Settings imported successfully")

        from src.utils.aws_utils import get_dynamodb_client
        print("✅ AWS utils imported successfully")

        from src.utils.mlflow_utils import setup_mlflow
        print("✅ MLflow utils imported successfully")

        from src.flows.model_training_flow import ModelTrainingFlow
        print("✅ Model training flow imported successfully")

        from src.flows.batch_prediction_flow import BatchPredictionFlow
        print("✅ Batch prediction flow imported successfully")

        return True
    except ImportError as e:
        print(f"❌ Import error: {e}")
        return False

def test_environment():
    """Test environment configuration"""
    print("\nEnvironment Configuration:")
    print(f"Python version: {sys.version}")
    print(f"Working directory: {os.getcwd()}")
    print(f"PYTHONPATH: {os.environ.get('PYTHONPATH', 'Not set')}")

    # Check if .env exists
    if os.path.exists('.env'):
        print("✅ .env file exists")
    else:
        print("⚠️  .env file not found (copy from env.example)")

def test_directories():
    """Test that required directories exist"""
    required_dirs = [
        'src/flows',
        'src/models',
        'src/utils',
        'src/config',
        'data/raw',
        'data/processed',
        'data/models',
        'ops/metaflow',
        'ops/mlflow',
        'ops/postgresql',
        'scripts',
        'docker'
    ]

    print("\nDirectory Structure:")
    for dir_path in required_dirs:
        if os.path.exists(dir_path):
            print(f"✅ {dir_path}")
        else:
            print(f"❌ {dir_path}")

def test_files():
    """Test that required files exist"""
    required_files = [
        'pyproject.toml',
        'mise.toml',
        'docker compose.yml',
        'env.example',
        'README.md',
        'src/flows/model_training_flow.py',
        'src/flows/batch_prediction_flow.py',
        'src/config/settings.py',
        'src/utils/aws_utils.py',
        'src/utils/mlflow_utils.py',
        'docker/Dockerfile.metaflow',
        'docker/Dockerfile.mlflow',
        'scripts/setup.sh',
        'scripts/run_training.sh',
        'scripts/run_prediction.sh'
    ]

    print("\nRequired Files:")
    for file_path in required_files:
        if os.path.exists(file_path):
            print(f"✅ {file_path}")
        else:
            print(f"❌ {file_path}")

def main():
    """Run all tests"""
    print("ML Infrastructure Setup Test")
    print("=" * 40)

    # Test imports
    imports_ok = test_imports()

    # Test environment
    test_environment()

    # Test directories
    test_directories()

    # Test files
    test_files()

    print("\n" + "=" * 40)
    if imports_ok:
        print("✅ Setup appears to be correct!")
        print("\nNext steps:")
        print("1. Copy env.example to .env and configure AWS credentials")
        print("2. Run: ./scripts/setup.sh")
        print("3. Run: ./scripts/run_training.sh")
    else:
        print("❌ Setup has issues. Please check the errors above.")

if __name__ == "__main__":
    main()
