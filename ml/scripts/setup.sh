#!/bin/bash
set -e

echo "Setting up ML infrastructure..."

# Create necessary directories
mkdir -p data/{raw,processed,models}
mkdir -p ops/{metaflow,mlflow,postgresql}/storage
mkdir -p logs

# Build and start services
echo "Building Docker images..."
docker compose build

echo "Starting MLflow and PostgreSQL..."
docker compose up
