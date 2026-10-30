const assert = require('node:assert/strict');
const {execFileSync, execFile} = require('node:child_process');
const fs = require('node:fs/promises');
const os = require('node:os');
const path = require('node:path');
const {promisify} = require('node:util');
const {test} = require('node:test');

const run = promisify(execFile);

async function deployment(t) {
  const cwd = await fs.mkdtemp(path.join(os.tmpdir(), 'web-deploy-'));
  t.after(() => fs.rm(cwd, {recursive: true, force: true}));
  for (const dir of ['bin', 'module-config', 'app-config', 'bundle']) {
    await fs.mkdir(path.join(cwd, dir));
  }
  await fs.copyFile(path.join(__dirname, 'deploy-app.sh'), path.join(cwd, 'deploy-app.sh'));
  await fs.writeFile(path.join(cwd, 'bundle', 'index.html'), '<html>Selected bundle</html>');
  execFileSync('zip', ['-q', path.join(cwd, 'selected.zip'), 'index.html'], {cwd: path.join(cwd, 'bundle')});
  for (const env of ['dev', 'ci']) {
    await fs.writeFile(path.join(cwd, 'module-config', `${env}.js`),
      `module.exports = {bucketName: '${env}-website', artifactBucket: 'artifacts', artifactBasePath: 'whatsdone-assets'};`);
    await fs.writeFile(path.join(cwd, 'app-config', `${env}.json`), JSON.stringify({
      API_ORIGIN: `https://${env}.example.test`, REGION: 'ap-southeast-2',
      USER_POOL_ID: `${env}-users`, IDENTITY_POOL_ID: `${env}-identities`, CLIENT_ID: `${env}-client`
    }));
  }
  await fs.writeFile(path.join(cwd, 'bin', 'aws'), `#!/usr/bin/env node
const fs = require('node:fs');
const path = require('node:path');
const args = process.argv.slice(2);
fs.appendFileSync('calls.jsonl', JSON.stringify(args) + '\\n');
if (args[0] !== 's3') process.exit(2);
if (args[1] === 'cp' && args[2].startsWith('s3://')) {
  if (process.env.MISSING_ARTIFACT) process.exit(1);
  fs.copyFileSync('selected.zip', args[3]);
} else if (args[1] === 'cp' && args[2] === '--recursive') {
  fs.cpSync(args[3], path.join('served', args[4].slice(5)), {recursive: true});
} else if (args[1] !== 'sync') process.exit(2);
`);
  await fs.chmod(path.join(cwd, 'bin', 'aws'), 0o755);
  return {
    cwd,
    deploy: (overrides = {}) => run('bash', ['deploy-app.sh'], {cwd, timeout: 10000,
      env: {...process.env, PATH: `${cwd}/bin:${process.env.PATH}`,
        ENV_NAME: 'dev', BUILD_NUMBER: 'selected-revision', npm_package_config_BUILD_DIR: '.build', ...overrides}}),
    calls: async () => (await fs.readFile(path.join(cwd, 'calls.jsonl'), 'utf8')).trim().split('\n').map(JSON.parse)
  };
}

test('web deployment promotes identical bundle bytes with each environment configuration', async t => {
  const fixture = await deployment(t);
  const archive = await fs.readFile(path.join(fixture.cwd, 'selected.zip'));
  for (const ENV_NAME of ['dev', 'ci']) {
    await fixture.deploy({ENV_NAME});
    const served = path.join(fixture.cwd, 'served', `${ENV_NAME}-website`);
    assert.equal(await fs.readFile(path.join(served, 'index.html'), 'utf8'), '<html>Selected bundle</html>');
    assert.deepEqual(await fs.readFile(path.join(served, 'appConfig.json')),
      await fs.readFile(path.join(fixture.cwd, 'app-config', `${ENV_NAME}.json`)));
  }
  assert.deepEqual(await fs.readFile(path.join(fixture.cwd, 'selected.zip')), archive);
  const downloads = (await fixture.calls()).filter(args => args[2]?.startsWith('s3://'));
  assert.deepEqual(downloads.map(args => args[2]), [
    's3://artifacts/whatsdone-assets/selected-revision.zip',
    's3://artifacts/whatsdone-assets/selected-revision.zip'
  ]);
});

test('missing web settings stop before any AWS command', async t => {
  const fixture = await deployment(t);
  await assert.rejects(fixture.deploy({BUILD_NUMBER: ''}), /BUILD_NUMBER is not set/);
  const app = path.join(fixture.cwd, 'app-config', 'dev.json');
  await fs.writeFile(app, '{}');
  await assert.rejects(fixture.deploy(), /Missing app setting API_ORIGIN/);
  await assert.rejects(fs.stat(path.join(fixture.cwd, 'calls.jsonl')), {code: 'ENOENT'});
});

test('missing web artifact never deploys a stale local ZIP', async t => {
  const fixture = await deployment(t);
  await fs.mkdir(path.join(fixture.cwd, '.build'));
  await fs.copyFile(path.join(fixture.cwd, 'selected.zip'),
    path.join(fixture.cwd, '.build', 'selected-revision.zip'));
  await assert.rejects(fixture.deploy({MISSING_ARTIFACT: '1'}));
  const calls = await fixture.calls();
  assert.equal(calls.length, 1);
  assert.equal(calls[0][2], 's3://artifacts/whatsdone-assets/selected-revision.zip');
  await assert.rejects(fs.stat(path.join(fixture.cwd, 'served')), {code: 'ENOENT'});
});
