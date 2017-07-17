
module.exports = {
  moduleName: 'test-cr',
  tasks: [
    {
      id: 'deploy-custom-resource',
      type: 'cf-stack',
      stackName: 'module',
      stackTemplate: {
        script: 'cp ./template.json $KUMO_TEMPLATE_OUTPUT_FILE'
      }
    }
  ]
}
