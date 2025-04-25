# AWS Comprehend Topic Classification Tools

This directory contains tools for managing AWS Comprehend custom classifier for topic classification of "done" entries.

## Prerequisites

1. AWS Account with appropriate permissions
2. AWS CLI configured with credentials
3. Python 3.13 or higher
4. [mise](https://mise.jdx.dev/) for tool management
5. uv package installer and resolver (install via mise)

## Environment Setup

1. Install uv using mise: `mise install`

2. Create a `mise.local.toml` file in this directory with the following variables:

   ```env
   [env]
   AWS_PROFILE = "your-profile-name"
   TRAINING_SPACE_BUCKET_URI = "s3://<your-bucket-name>"
   AWS_COMPREHEND_ROLE_ARN = "arn:aws:iam::<your-account>:role/<your-comprehend-role>"
   ```

## Training Data Format

The training data should be in CSV format with the following structure:
- Each row represents one document
- The CSV should have two columns:
  1. `label`: The topic category for the entry
  2. `text`: The content of the "done" entry

Example:

```csv
label,text
"Development","Completed the user authentication module"
"Meeting","Had a team meeting to discuss project timeline"
```

## Usage

1. Install dependencies using uv:

   ```sh
   uv sync
   ```

2. Create and train a new classifier. It took 40 mins for just 36 samples:

   ```
   $ python manage_classifier.py
   Creating classifier with training data: s3://your-bucket-name/training-data
   Classifier creation initiated. ARN: <your-classifier-arn>
   Waiting for training to complete...
   Classifier status: SUBMITTED
   Training in progress... Checking again in 300 seconds
   Classifier status: TRAINING
   Training in progress... Checking again in 300 seconds
   Classifier status: TRAINING
   Training in progress... Checking again in 300 seconds
   Classifier status: TRAINING
   Training in progress... Checking again in 300 seconds
   Classifier status: TRAINING
   Training in progress... Checking again in 300 seconds
   Classifier status: TRAINING
   Training in progress... Checking again in 300 seconds
   Classifier status: TRAINING
   Training in progress... Checking again in 300 seconds
   Classifier status: TRAINING
   Training in progress... Checking again in 300 seconds
   Classifier status: TRAINED
   Training completed with status: TRAINED
   ```

## Notes

- Training a custom classifier can take several hours depending on the amount of training data
- The classifier name is set to `whatsdone-topic-classifier` by default
- Make sure your AWS IAM role has the necessary permissions for Comprehend operations
- The training data should be properly formatted and contain enough examples for each topic category
- Dependencies are managed through `pyproject.toml` for modern Python packaging
