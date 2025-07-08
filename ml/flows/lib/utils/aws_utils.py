import boto3
import os
import sys
from typing import List, Dict, Any
sys.path.append(os.path.dirname(os.path.dirname(__file__)))
from config.settings import settings

def get_dynamodb_client():
    """Get DynamoDB client"""
    return boto3.client('dynamodb', region_name=settings.AWS_REGION)

def get_dynamodb_resource():
    """Get DynamoDB resource"""
    return boto3.resource('dynamodb', region_name=settings.AWS_REGION)

def get_s3_client():
    """Get S3 client"""
    return boto3.client('s3', region_name=settings.AWS_REGION)

def load_data_from_s3(bucket: str, key: str) -> List[Dict[str, Any]]:
    """Load data from S3"""
    s3_client = get_s3_client()

    try:
        # Download data from S3
        s3_client.download_file(bucket, key, '/tmp/done_items.csv')
        import pandas as pd
        df = pd.read_csv('/tmp/done_items.csv')
        return df.to_dict('records')
    except Exception as e:
        print(f"Error loading from S3: {e}")
        return []

def scan_dynamodb_table(table_name: str) -> List[Dict[str, Any]]:
    """Scan all items from DynamoDB table"""
    dynamodb = get_dynamodb_resource()
    table = dynamodb.Table(table_name)

    items = []
    scan_kwargs = {}

    while True:
        response = table.scan(**scan_kwargs)
        items.extend(response.get('Items', []))
        last_evaluated_key = response.get('LastEvaluatedKey')
        if not last_evaluated_key:
            break
        scan_kwargs['ExclusiveStartKey'] = last_evaluated_key

    return items

def update_dynamodb_item(table_name: str, item_id: str, topics: List[str], timestamp: str):
    """Update DynamoDB item with topics"""
    dynamodb = get_dynamodb_resource()
    table = dynamodb.Table(table_name)

    try:
        table.update_item(
            Key={'id': item_id},
            UpdateExpression='SET topics = :topics, last_updated = :timestamp',
            ExpressionAttributeValues={
                ':topics': topics,
                ':timestamp': timestamp
            }
        )
        return True
    except Exception as e:
        print(f"Error updating item {item_id}: {e}")
        return False
