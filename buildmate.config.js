
module.exports = {
  tasks: [
    // Build modules
    {
      description: 'Build whatsdone-assets',
      path: /^(modules\/whatsdone-assets)\/.*/,
      command: `
        cd $BM_PATH_VAR_1 \\
            && npm install \\
            && npm run lint`
    },
    {
      description: 'Build whatsdone-app',
      path: /^(modules\/whatsdone-app)\/.*/,
      command: `
        cd $BM_PATH_VAR_1/src \\
            && npm install \\
            && npm run lint \\
            && npm test \\
            && npm run report-coverage`
    },

    // Build tools
    {
      description: 'Build tools/copy-done-table',
      path: /^(tools\/copy-done-table)\/.*/,
      command: 'cd $BM_PATH_VAR_1 && make test'
    },

    // Deploy modules
    {
      description: 'Deploy module',
      path: /^(modules\/[^/]+)\/.*/,
      command: 'cd $BM_PATH_VAR_1 && npm run deploy -- --env $ENV_NAME --region $AWS_REGION'
    },
    {
      description: 'Deploy whatsdone-assets app',
      path: /^(modules\/whatsdone-assets)\/.*/,
      command: 'cd $BM_PATH_VAR_1 && npm install && npm run build && npm run deploy:app'
    }
  ]
};
