#!/usr/bin/env python3
import os
from aws_cdk import App, Environment, Stack
from topic_classification_stack import TopicClassificationStack

app = App()

TopicClassificationStack(
    app,
    "WhatsdoneTopicClassificationStack",
    stack_name="whatsdone-topic-classification",
    env=Environment(
        region=os.getenv("AWS_REGION", "ap-southeast-2"),
    ),
)

app.synth()
