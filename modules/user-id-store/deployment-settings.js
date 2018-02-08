
module.exports = {
  moduleName: 'user-id-store',
  config: {
    script: 'node -p "JSON.stringify(require(\'./module-config/$ENV.js\'))"',
    envVars: {
      ENV: {$ref: '#/_env'}
    }
  },
  outputsStore: {
    type: 's3-bucket',
    region: {$ref: '#/_args/region'},
    bucket: {'$ref': '#/_deploymentConfig/deploymentOutputsBucket'},
    prefix: {'Fn::Join': ['/', [{$ref: '#/_env'}, 'user-id-store']]}
  },
  tasks: [
    {
      id: 'create-user-table',
      type: 'cf-stack',
      stackName: {'Fn::Join': ['-', ['whatsdone', {$ref: '#/_env'}, 'user-id-store']]},
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
