#!/bin/bash
set -e

echo "Setting up ML infrastructure..."

# Create necessary directories
mkdir -p data/{raw,processed,models}
mkdir -p ops/{metaflow,mlflow,postgresql}/storage
mkdir -p logs

# Copy AWS credentials if they exist
if [ -f ~/.aws/credentials ]; then
    echo "AWS credentials found"
else
    echo "Warning: AWS credentials not found. Please configure AWS access."
fi

# Build and start services
echo "Building Docker images..."
docker compose build

echo "Starting MLflow and PostgreSQL..."
docker compose up -d postgres mlflow

# Wait for services to be ready
echo "Waiting for services to be ready..."
sleep 30

# Check if MLflow is running
if curl -f http://localhost:5001 > /dev/null 2>&1; then
    echo "MLflow is running at http://localhost:5001"
else
    echo "Warning: MLflow might not be ready yet"
fi

echo "ML infrastructure setup completed!"
echo ""
echo "Next steps:"
echo "1. Run model training: ./scripts/run_training.sh"
echo "2. Run batch prediction: ./scripts/run_prediction.sh"
echo "3. Check MLflow at http://localhost:5001 for experiment results"
