
module.exports = {
  tasks: [
    {
      path: /^(modules\/env\/whatsdone-assets)\/.*/,
      command: `
        cd modules/env/whatsdone-assets \\
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
      command: 'cd tools/buildman && yarn install && yarn run build'
    }
  ]
};
