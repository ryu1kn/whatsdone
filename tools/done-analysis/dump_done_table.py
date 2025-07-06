import argparse
import boto3
import json
from decimal import Decimal
from typing import Any


def decimal_default(obj: Any):
    if isinstance(obj, Decimal):
        return float(obj)
    raise TypeError


def dump_dynamodb_table(table_name: str, region: str, output_file: str):
    dynamodb = boto3.resource('dynamodb', region_name=region)
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

    with open(output_file, 'w') as f:
        json.dump(items, f, default=decimal_default, indent=2, ensure_ascii=False)
    print(f"Dumped {len(items)} items from {table_name} to {output_file}")


def main():
    parser = argparse.ArgumentParser(description="Dump all items from a DynamoDB table to a JSON file.")
    parser.add_argument('--table', required=True, help='DynamoDB table name')
    parser.add_argument('--region', required=True, help='AWS region')
    parser.add_argument('--output', required=True, help='Output JSON file')
    args = parser.parse_args()

    dump_dynamodb_table(args.table, args.region, args.output)


if __name__ == '__main__':
    main()
