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
    bucket: {$ref: '#/_deploymentConfig/deploymentOutputsBucket'},
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
        IdentityPoolName: {$ref: '#/_deploymentConfig/identityPoolName'},
        UserPoolName: {$ref: '#/_deploymentConfig/userPoolName'},
        ExplicitAuthFlows: {$ref: '#/_deploymentConfig/explicitAuthFlows'},
        CallbackUrls: {$ref: '#/_deploymentConfig/callbackUrls'}
      }
    },
    {
      id: 'create-domain-for-user-pool',
      type: 'custom',
      run: {
        script: './create-domain-for-userpool.sh',
        envVars: {
          TASK_REGION: {$ref: '#/_args/region'},
          USER_POOL_ID: {$ref: '#/_deploymentOutputs/UserPoolId'},
          USER_POOL_DOMAIN: {$ref: '#/_deploymentConfig/userPoolDomain'}
        }
      }
    }
  ]
};
