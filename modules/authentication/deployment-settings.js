module.exports = {
  moduleName: 'authentication',
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
    prefix: {'Fn::Join': ['/', [{$ref: '#/_env'}, 'authentication']]}
  },
  tasks: [
    {
      id: 'create-user-pool',
      type: 'cf-stack',
      stackName: {
        'Fn::Join': ['-', ['whatsdone', {$ref: '#/_env'}, 'authentication']]
      },
      stackTemplate: {
        script: 'cp ./template.json $TEMPLATE_OUTPUT_FILE',
        envVars: {
          TEMPLATE_OUTPUT_FILE: {$ref: '#/_templateOutputFile'}
        }
      },
      stackParams: {
        UserPoolName: {$ref: '#/_deploymentConfig/userPoolName'}
      }
    }
  ]
};
