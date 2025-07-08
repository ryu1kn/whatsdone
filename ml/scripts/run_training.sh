#!/bin/bash
set -e

echo "Running model training flow..."

# Run the training flow in Docker
docker compose run --rm metaflow python -m src.flows.model_training_flow

echo "Model training completed!"
echo ""
echo "Check MLflow at http://localhost:5000 for experiment results"
