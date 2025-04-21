# AWS CDK Infrastructure for Topic Classification

This directory contains AWS CDK code to create the necessary AWS resources for the topic classification project.

## Prerequisites

1. AWS CDK CLI installed: `mise install`
2. AWS credentials configured in your AWS config/credentials file with profile `ryuichi`
3. Python dependencies installed: `uv sync`

## Deployment

First, ensure you are in the ops directory:

```sh
cd tools/topic-classification/ops
```

1. Bootstrap CDK (only needed once per account/region):

   ```sh
   cdk bootstrap
   ```

2. Deploy the infrastructure:

   ```sh
   cdk deploy
   ```

3. After deployment, note the outputs:
   - `WhatsdoneTopicClassificationBucketName`: The name of the S3 bucket for training data
   - `WhatsdoneTopicClassificationRoleArn`: The ARN of the IAM role for Comprehend

4. Make the above values available as environment variables (you can use `mise.local.toml` like [this](https://mise.jdx.dev/environments/#environments) for example)

   ```env
   TRAINING_SPACE_BUCKET_URI=s3://<WhatsdoneTopicClassificationBucketName>
   AWS_COMPREHEND_ROLE_ARN=<WhatsdoneTopicClassificationRoleArn>
   ```

## Cleanup

To destroy all resources:

```sh
cdk destroy
```

## Notes

- The S3 bucket is configured with `RemovalPolicy.DESTROY` for development. Change to `RETAIN` for production.
- The IAM role has permissions for S3 and CloudWatch Logs. Consider restricting the CloudWatch Logs permissions in production.
- All CDK commands must be run from the `ops` directory where `cdk.json` is located
