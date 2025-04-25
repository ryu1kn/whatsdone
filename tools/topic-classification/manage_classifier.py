import boto3
import os
import time
import datetime

class ComprehendClassifierManager:
    def __init__(self, training_space_bucket_name, aws_comprehend_role_arn, current_commit_hash):
        self.comprehend = boto3.client('comprehend', region_name=os.getenv('AWS_REGION', 'ap-southeast-2'))
        self.s3 = boto3.client('s3', region_name=os.getenv('AWS_REGION', 'ap-southeast-2'))
        self.classifier_name = "whatsdone-topic-classifier"
        self.training_space_bucket_name = training_space_bucket_name
        self.classifier_version_name = datetime.datetime.now().strftime("%Y-%m-%dT%H-%M-%S") + "--" + current_commit_hash
        self.role_arn = aws_comprehend_role_arn
        self.classifier_arn = None

    def upload_training_data(self):
        print(f"Uploading the training data to s3://{self.training_space_bucket_name}/{self.classifier_version_name}/input/")
        self.s3.upload_file(
            "./training-data.csv",
            self.training_space_bucket_name,
            f"{self.classifier_version_name}/input/training-data.csv"
        )

    def _list_classifiers(self):
        response = self.comprehend.list_document_classifiers()
        return response['DocumentClassifierPropertiesList']

    def create_classifier(self):
        response = self.comprehend.create_document_classifier(
            DocumentClassifierName=self.classifier_name,
            VersionName=self.classifier_version_name,
            LanguageCode="en",
            DataAccessRoleArn=self.role_arn,
            InputDataConfig={
                'S3Uri': f"s3://{self.training_space_bucket_name}/{self.classifier_version_name}/input",
                'DocumentType': 'PLAIN_TEXT_DOCUMENT'
            },
            OutputDataConfig={
                'S3Uri': f"s3://{self.training_space_bucket_name}/{self.classifier_version_name}/output"
            }
        )
        print(f"Classifier creation initiated. ARN: {response['DocumentClassifierArn']}")
        self.classifier_arn = response['DocumentClassifierArn']

    def _check_classifier_status(self):
        response = self.comprehend.describe_document_classifier(DocumentClassifierArn=self.classifier_arn)
        status = response['DocumentClassifierProperties']['Status']
        print(f"Classifier status: {status}")
        return status

    def wait_for_training_completion(self, check_interval=60):
        while True:
            status = self._check_classifier_status()
            if status in ['TRAINED', 'FAILED', 'STOPPED']:
                return status
            if status == 'STOP_REQUESTED':
                print("Training stop requested. Waiting for the classifier to stop...")
            else:
                print(f"Training in progress... Checking again in {check_interval} seconds")
            time.sleep(check_interval)

    def delete_if_already_exists(self):
        for classifier in self._list_classifiers():
            if classifier['DocumentClassifierArn'].endswith("/" + self.classifier_name):
                classifier_arn = classifier['DocumentClassifierArn']
                print(f"Deleting existing classifier: {classifier_arn}")
                self.comprehend.delete_document_classifier(DocumentClassifierArn=classifier_arn)
                print(f"Classifier {classifier_arn} deleted successfully")

                # Wait a bit to ensure the classifier is deleted
                time.sleep(30)
                break


if __name__ == "__main__":
    manager = ComprehendClassifierManager(
        training_space_bucket_name=os.getenv('TRAINING_SPACE_BUCKET_NAME'),
        aws_comprehend_role_arn=os.getenv('AWS_COMPREHEND_ROLE_ARN'),
        current_commit_hash=os.popen('git rev-parse --short HEAD').read().strip(),
    )

    # manager.delete_if_already_exists()
    manager.upload_training_data()

    print(f"Creating classifier with training data")
    manager.create_classifier()

    print("Waiting for training to complete...")
    status = manager.wait_for_training_completion()

    print(f"Training completed with status: {status}")
