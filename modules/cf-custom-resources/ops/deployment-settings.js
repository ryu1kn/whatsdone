
module.exports = {
  moduleName: 'cf-custom-resources',
  config: {
    script: "node -p \"JSON.stringify(require('../module-config.js'))\""
  },
  outputsStore: {
    type: 's3-bucket',
    region: {$ref: '#/_args/region'},
    bucket: 'whatsdone-deployment-outputs',
    prefix: 'cf-custom-resources'
  },
  tasks: [
    {
      id: 'build-upload-path',
      type: 'custom',
      run: {
        script: 'echo \\\"$S3_BASE_PATH/$BUILD_NUMBER.zip\\\" > $TASK_OUTPUTS_FILE',
        envVars: {
          TASK_OUTPUTS_FILE: {$ref: '#/_taskOutputsFile'},
          S3_BASE_PATH: {$ref: '#/_deploymentConfig/artifactBasePath'}
        }
      },
      outputsName: 'S3Key'
    },
    {
      id: 'deploy-lambda',
      type: 'cf-stack',
      stackName: 'cf-custom-resources-module',
      stackTemplate: {
        script: 'cp ./template.json $TEMPLATE_OUTPUT_FILE',
        envVars: {
          TEMPLATE_OUTPUT_FILE: {$ref: '#/_templateOutputFile'}
        }
      },
      stackParams: {
        S3Bucket: {$ref: '#/_deploymentConfig/artifactBucket'},
        S3Key: {$ref: '#/_deploymentOutputs/S3Key'}
      }
    }
  ]
}
