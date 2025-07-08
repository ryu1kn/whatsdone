# ML Infrastructure for What's Done

### Setup Environment

```bash
cp env.example .env
```

### Setup Infrastructure

```bash
# Run setup script
./scripts/setup.sh
```

This will:
- Create necessary directories
- Build Docker images
- Start PostgreSQL and MLflow services
- Verify services are running

### Run a flow

```bash
python flows/model_training_flow.py run
```

## Directory Structure

```
ml/
├── flows/               # Metaflow pipelines
├── data/                # Data storage
├── ops/                 # Infrastructure config
├── docker/              # Docker images
└── scripts/             # Scripts
```
