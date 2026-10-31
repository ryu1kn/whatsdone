const assert = require('node:assert/strict');
const {execFile} = require('node:child_process');
const fs = require('node:fs/promises');
const os = require('node:os');
const path = require('node:path');
const {promisify} = require('node:util');
const test = require('node:test');
const run = promisify(execFile);

async function fixture(t) {
  const cwd = await fs.mkdtemp(path.join(os.tmpdir(), 'ci-build-'));
  t.after(() => fs.rm(cwd, {recursive: true, force: true}));
  for (const dir of ['bin', 'modules/backend', 'modules/api', 'modules/web-assets']) {
    await fs.mkdir(path.join(cwd, dir), {recursive: true});
  }
  for (const file of ['ci-build.sh', 'deploy-module.sh', 'mise.toml']) {
    await fs.copyFile(path.join(__dirname, file), path.join(cwd, file));
  }
  for (const command of ['aws', 'yarn', 'kumo']) {
    const script = `#!/bin/bash
set -eu
printf '%s %s %s\\n' "${command}" "\${PWD##*/}" "$*" >> "$CALLS"
printf '%s\\n' "$BUILD_NUMBER" >> "$BUILDS"
if [[ '${command}' == kumo ]]; then mkdir -p .kumo; echo '{}' > .kumo/package.json; fi
`;
    await fs.writeFile(path.join(cwd, 'bin', command), script, {mode: 0o755});
  }
  const calls = path.join(cwd, 'calls');
  return {
    deploy: (env = {}, args = ['--deploy-selected-artifacts'], throughMise = false) => run(throughMise ? 'mise' : 'bash',
      throughMise ? ['exec', '--', 'env', `PATH=${cwd}/bin:${process.env.PATH}`, 'bash', 'ci-build.sh', ...args] : ['ci-build.sh', ...args], {
      cwd, timeout: 30000,
      env: {...process.env, PATH: `${cwd}/bin:${process.env.PATH}`, CALLS: calls, BUILDS: path.join(cwd, 'builds'), MISE_TRUSTED_CONFIG_PATHS: cwd, MISE_STATE_DIR: path.join(cwd, 'mise-state'),
        AWS_PROFILE: 'ci-whatsdone', AWS_REGION: 'ap-southeast-2',
        BUILD_BRANCH: 'main-new', BUILD_NUMBER: 'selected-build', ENV_NAME: 'ci', ...env}
    }),
    calls: () => fs.readFile(calls, 'utf8'),
    builds: () => fs.readFile(path.join(cwd, 'builds'), 'utf8')
  };
}

test('selected artifacts deploy to CI without building or publishing', async t => {
  const f = await fixture(t);
  await f.deploy();
  const calls = await f.calls();
  assert.match(calls, /yarn backend run deploy --env ci --region ap-southeast-2/);
  assert.match(calls, /yarn api run deploy --env ci --region ap-southeast-2/);
  assert.match(calls, /yarn web-assets run deploy:app/);
  assert.doesNotMatch(calls, /publish-artifact|run build|buildmate/);
});

test('selected deployment rejects production, unsupported refs and invalid artifact selection before AWS', async t => {
  for (const env of [{ENV_NAME: 'prod'}, {BUILD_BRANCH: 'feature'}, {BUILD_NUMBER: ''},
    {BUILD_NUMBER: '../other'}, {BUILD_NUMBER: '0'}]) {
    const f = await fixture(t);
    await assert.rejects(f.deploy(env), /Selected deployment requires/);
    await assert.rejects(f.calls(), {code: 'ENOENT'});
  }
});


test('CI deployment keeps the selected build when launched through mise', async t => {
  const f = await fixture(t);
  await f.deploy({}, undefined, true);
  const builds = (await f.builds()).trim().split('\n');
  assert.ok(builds.length > 0);
  assert.ok(builds.every(build => build === 'selected-build'));
});
