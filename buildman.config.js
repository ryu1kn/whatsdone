
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
        cd modules/env/whatsdone-app/src \\
            && npm install \\
            && npm run lint \\
            && npm test \\
            && npm run report-coverage`
    },
    {
      path: /^tools\/buildman\/.*/,
      command: 'cd tools/buildman && npm install && npm run build'
    }
  ]
};
