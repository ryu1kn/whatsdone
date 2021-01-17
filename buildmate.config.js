
module.exports = {
  tasks: [
    {
      description: "Deploy to CI environment",
      path: /^modules\/.*/,
      command: `
        BUILD_NUMBER="$TRAVIS_BUILD_NUMBER" ENV_NAME=ci ./deploy-system.sh`
    },
    {
      description: "Run end-to-end tests on CI environment",
      path: /^(modules|test\/e2e)\/.*/,
      command: 'cd test/e2e && make test'
    },
    {
      description: "Check if docker is available",
      path: /.*/,
      command: 'command -v docker && command -v docker-compose'
    },
    {
      description: "Deploy to Production environment",
      path: /^modules\/.*/,
      command: `
        BUILD_NUMBER="$TRAVIS_BUILD_NUMBER" ENV_NAME=prod ./deploy-system.sh`
    },

    // Build tools
    {
      description: 'Build tools/copy-done-table',
      path: /^(tools\/copy-done-table)\/.*/,
      command: 'cd $BM_PATH_VAR_1 && make test'
    }
  ]
};
