#!/bin/bash
set -e

echo "Running batch prediction flow..."

# Run the prediction flow in Docker
docker compose run --rm metaflow python -m src.flows.batch_prediction_flow

echo "Batch prediction completed!"
echo ""
echo "Check MLflow at http://localhost:5000 for batch results"
