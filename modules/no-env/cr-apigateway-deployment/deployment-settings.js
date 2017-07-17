
module.exports = {
  moduleName: 'cr-apigateway-deployment',
  config: {
    script: "node -p \"JSON.stringify(require('./module-config.js'))\""
  },
  tasks: [
    {
      id: 'provision-lambda',
      type: 'custom',
      run: {
        script: `node upload-lambda --target-dir src \\
                   --s3bucket $S3_BUCKET --s3key cr-apigateway-deployment/$BUILD_NUMBER.zip \\
                   > $KUMO_TASK_OUTPUTS_FILE`,
        envVars: {
          S3_BUCKET: {$ref: '#/_deploymentConfig/artifactBucket'}
        }
      }
    },
    {
      id: 'deploy-lambda',
      type: 'cf-stack',
      stackName: 'module',
      stackTemplate: {
        script: 'cp ./template.json $KUMO_TEMPLATE_OUTPUT_FILE'
      },
      stackParams: {
        S3Bucket: {$ref: '#/_deploymentOutputs/cr-apigateway-deployment/S3Bucket'},
        S3Key: {$ref: '#/_deploymentOutputs/cr-apigateway-deployment/S3Key'}
      }
    }
  ]
}
