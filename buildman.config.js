
module.exports = {
  tasks: [
    {
      path: /^(modules\/env\/whatsdone-assets)\/.*/,
      command: `
        cd $BM_PATH_VAR_1 \\
            && npm install \\
            && npm run lint`
    },
    {
      path: /^(modules\/env\/whatsdone-app)\/.*/,
      command: `
        cd $BM_PATH_VAR_1/src \\
            && npm install \\
            && npm run lint \\
            && npm test \\
            && npm run report-coverage`
    },
    {
      path: /^(tools\/buildman)\/.*/,
      command: 'cd $BM_PATH_VAR_1 && yarn install && yarn run build'
    }
  ]
};
