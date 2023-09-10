
module.exports = {
  tasks: [
    {
      description: "Deploy to ENV_NAME environment",
      path: /^modules\/.*/,
      command: './deploy-system.sh'
    },

    // Build tools (to be migrated to GitHub Actions)
    {
      description: 'Build tools/copy-done-table',
      path: /^(tools\/copy-done-table)\/.*/,
      command: 'cd $BM_PATH_VAR_1 && make test'
    }
  ]
};
