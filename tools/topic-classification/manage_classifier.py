import boto3
import os
import time

class ComprehendClassifierManager:
    def __init__(self, training_space_bucket_uri, aws_comprehend_role_arn):
        self.comprehend = boto3.client('comprehend', region_name=os.getenv('AWS_REGION', 'ap-southeast-2'))
        self.classifier_name = "whatsdone-topic-classifier"
        self.training_space_bucket_uri = training_space_bucket_uri
        self.role_arn = aws_comprehend_role_arn
        self.classifier_arn = None

    def _list_classifiers(self):
        response = self.comprehend.list_document_classifiers()
        return response['DocumentClassifierPropertiesList']

    def create_classifier(self):
        response = self.comprehend.create_document_classifier(
            DocumentClassifierName=self.classifier_name,
            LanguageCode="en",
            DataAccessRoleArn=self.role_arn,
            InputDataConfig={
                'S3Uri': self.training_space_bucket_uri + "/input",
                'DocumentType': 'PLAIN_TEXT_DOCUMENT'
            },
            OutputDataConfig={
                'S3Uri': self.training_space_bucket_uri + "/output"
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
        training_space_bucket_uri=os.getenv('TRAINING_SPACE_BUCKET_URI'),
        aws_comprehend_role_arn=os.getenv('AWS_COMPREHEND_ROLE_ARN')
    )

    manager.delete_if_already_exists()

    print(f"Creating classifier with training data")
    manager.create_classifier()

    print("Waiting for training to complete...")
    status = manager.wait_for_training_completion()

    print(f"Training completed with status: {status}")
