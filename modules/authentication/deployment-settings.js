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
        UserPoolName: {$ref: '#/_deploymentConfig/userPoolName'}
      }
    },
    {
      id: 'configure-user-pool',
      type: 'custom',
      run: {
        script: `aws cognito-idp update-user-pool-client \\
                    --user-pool-id $USER_POOL_ID \\
                    --client-id $USER_POOL_CLIENT_ID \\
                    --refresh-token-validity 1 \\
                    --supported-identity-providers "COGNITO" \\
                    --allowed-o-auth-flows-user-pool-client \\
                    --allowed-o-auth-flows "code" "implicit" \\
                    --allowed-o-auth-scopes "openid" \\
                    --callback-urls $CALLBACK_URLS
                `,
        envVars: {
          USER_POOL_ID: {$ref: '#/_deploymentOutputs/UserPoolId'},
          USER_POOL_CLIENT_ID: {$ref: '#/_deploymentOutputs/UserPoolClientId'},
          CALLBACK_URLS: {$ref: '#/_deploymentConfig/callbackUrls'}
        }
      }
    }
  ]
};
