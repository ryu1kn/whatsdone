
module.exports = {
  moduleName: 'user-store',
  config: {
    script: 'node -p "JSON.stringify(require(\'./module-config/$ENV.js\'))"',
    envVars: {
      ENV: {$ref: '#/_env'}
    }
  },
  outputsStore: {
    type: 's3-bucket',
    region: {$ref: '#/_args/region'},
    bucket: 'whatsdone-deployment-outputs',
    prefix: {'Fn::Join': ['/', [{$ref: '#/_env'}, 'user-store']]}
  },
  tasks: [
    {
      id: 'create-user-table',
      type: 'cf-stack',
      stackName: {'Fn::Join': ['-', [{$ref: '#/_env'}, 'user-store']]},
      stackTemplate: {
        script: 'cp ./template.json $TEMPLATE_OUTPUT_FILE',
        envVars: {
          TEMPLATE_OUTPUT_FILE: {$ref: '#/_templateOutputFile'}
        }
      },
      stackParams: {
        TableName: {$ref: '#/_deploymentConfig/tableName'},
        TableReadCapacity: {$ref: '#/_deploymentConfig/readCapacity'},
        TableWriteCapacity: {$ref: '#/_deploymentConfig/writeCapacity'}
      }
    }
  ]
};
