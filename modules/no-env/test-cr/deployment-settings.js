
module.exports = {
  moduleName: 'test-cr',
  tasks: [
    {
      id: 'deploy-custom-resource',
      type: 'cf-stack',
      stackName: 'module',
      stackTemplate: {
        script: 'cp ./template.json $TEMPLATE_OUTPUT_FILE',
        envVars: {
          TEMPLATE_OUTPUT_FILE: {$ref: '#/_templateOutputFile'}
        }
      }
    }
  ]
}
