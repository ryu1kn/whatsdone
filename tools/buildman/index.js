
const readLines = require('./lib/read-lines');
const TaskListExecutor = require('./lib/task-list-executor');
const TaskExecutor = require('./lib/task-executor');
const CommandExecutor = require('./lib/command-executor');

module.exports = async ({config, spawnSync, stdin, stdout, stderr, envVars, logger}) => {
  const commandExecutor = new CommandExecutor({spawnSync, envVars, stdout, stderr});
  const taskExecutor = new TaskExecutor({commandExecutor, logger});
  const taskListExecutor = new TaskListExecutor({taskExecutor});
  taskListExecutor.execute({
    tasks: config.tasks,
    filePaths: await readLines(stdin)
  });
};
