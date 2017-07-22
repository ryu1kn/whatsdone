
module.exports = {
  moduleName: 'cf-custom-resources',
  config: {
    script: "node -p \"JSON.stringify(require('../module-config.js'))\""
  },
  tasks: [
    {
      id: 'build-upload-path',
      type: 'custom',
      run: {
        script: 'echo \\\"$S3_BASE_PATH/$BUILD_NUMBER.zip\\\" > $KUMO_TASK_OUTPUTS_FILE',
        envVars: {
          S3_BASE_PATH: {$ref: '#/_deploymentConfig/artifactBasePath'}
        }
      },
      outputsName: 'S3Key'
    },
    {
      id: 'deploy-lambda',
      type: 'cf-stack',
      stackName: 'module',
      stackTemplate: {
        script: 'cp ./template.json $KUMO_TEMPLATE_OUTPUT_FILE'
      },
      stackParams: {
        S3Bucket: {$ref: '#/_deploymentConfig/artifactBucket'},
        S3Key: {$ref: '#/_deploymentOutputs/cf-custom-resources/S3Key'}
      }
    }
  ]
}