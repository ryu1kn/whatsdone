from aws_cdk import (
    Stack,
    aws_s3 as s3,
    aws_iam as iam,
    RemovalPolicy,
    CfnOutput,
)
from constructs import Construct


class TopicClassificationStack(Stack):
    def __init__(self, scope: Construct, construct_id: str, **kwargs) -> None:
        super().__init__(scope, construct_id, **kwargs)

        # Create an S3 bucket for training data
        self.training_bucket = s3.Bucket(
            self,
            "WhatsdoneTopicClassificationBucket",
            removal_policy=RemovalPolicy.DESTROY,  # For development, change to RETAIN for production
            auto_delete_objects=True,  # For development, remove for production
        )

        # Create an IAM role for Comprehend
        self.comprehend_role = iam.Role(
            self,
            "WhatsdoneTopicClassificationRole",
            assumed_by=iam.ServicePrincipal("comprehend.amazonaws.com"),
            description="Role for Whatsdone topic classification Comprehend custom classifier",
        )

        # Add permissions to the role
        self.comprehend_role.add_to_policy(
            iam.PolicyStatement(
                effect=iam.Effect.ALLOW,
                actions=[
                    "s3:GetObject",
                    "s3:ListBucket",
                    "s3:PutObject",
                ],
                resources=[
                    self.training_bucket.bucket_arn,
                    f"{self.training_bucket.bucket_arn}/*",
                ],
            )
        )

        # Add CloudWatch Logs permissions
        self.comprehend_role.add_to_policy(
            iam.PolicyStatement(
                effect=iam.Effect.ALLOW,
                actions=[
                    "logs:CreateLogGroup",
                    "logs:CreateLogStream",
                    "logs:PutLogEvents",
                ],
                resources=["*"],  # Consider restricting to specific log groups in production
            )
        )

        # Output the bucket name and role ARN
        CfnOutput(
            self,
            "WhatsdoneTopicClassificationBucketName",
            value=self.training_bucket.bucket_name,
            description="Name of the S3 bucket for Whatsdone topic classification training data",
        )

        CfnOutput(
            self,
            "WhatsdoneTopicClassificationRoleArn",
            value=self.comprehend_role.role_arn,
            description="ARN of the IAM role for Whatsdone topic classification Comprehend",
        )
