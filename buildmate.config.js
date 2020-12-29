
module.exports = {
  tasks: [
    // What's Done
    {
      description: "Build & Deploy What's Done",
      path: /^modules\/.*/,
      command: `
        BUILD_NUMBER="$TRAVIS_BUILD_NUMBER" ./deploy-system.sh`
    },

    // Build tools
    {
      description: 'Build tools/copy-done-table',
      path: /^(tools\/copy-done-table)\/.*/,
      command: 'cd $BM_PATH_VAR_1 && make test'
    }
  ]
};
