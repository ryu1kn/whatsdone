
module.exports = {
  tasks: [
    // Build modules
    {
      description: 'Build whatsdone-assets',
      path: /^(modules\/whatsdone-assets)\/.*/,
      command: `
        cd $BM_PATH_VAR_1 \\
            && yarn install \\
            && yarn run lint`
    },
    {
      description: 'Build whatsdone-app',
      path: /^(modules\/whatsdone-app)\/.*/,
      command: `
        cd $BM_PATH_VAR_1/src \\
            && yarn install \\
            && yarn run lint \\
            && yarn test \\
            && yarn run report-coverage`
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
      command: 'cd $BM_PATH_VAR_1 && yarn run deploy --env $ENV_NAME --region $AWS_REGION'
    },
    {
      description: 'Deploy API',  // XXX: whatsdone-api gets deployed twice if both -app and -api are updated
      path: /^(modules)\/whatsdone-app\/.*/,
      command: 'cd $BM_PATH_VAR_1/whatsdone-api && yarn run deploy --env $ENV_NAME --region $AWS_REGION'
    },
    {
      description: 'Deploy whatsdone-assets app',
      path: /^(modules\/whatsdone-assets)\/.*/,
      command: 'cd $BM_PATH_VAR_1 && yarn install && yarn run build && yarn run deploy:app'
    }
  ]
};
