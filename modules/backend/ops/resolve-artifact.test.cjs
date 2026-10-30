const assert = require('node:assert/strict');
const {execFile} = require('node:child_process');
const http = require('node:http');
const {promisify} = require('node:util');
const {test} = require('node:test');

const run = promisify(execFile);

function resolveArtifact(server, overrides = {}) {
  return run(process.execPath, ['resolve-artifact.js'], {
    cwd: __dirname,
    timeout: 10000,
    env: {...process.env,
      AWS_PROFILE: '', AWS_ACCESS_KEY_ID: 'test', AWS_SECRET_ACCESS_KEY: 'test',
      AWS_SESSION_TOKEN: '', AWS_REGION: 'ap-southeast-2', AWS_MAX_ATTEMPTS: '1',
      AWS_ENDPOINT_URL_S3: `http://127.0.0.1:${server.address().port}`,
      BUILD_NUMBER: 'selected-revision', ARTIFACT_BUCKET: 'artifacts',
      ARTIFACT_BASE_PATH: 'whatsdone-app', ...overrides}
  });
}

test('deployment resolves the same published backend ZIP for both environments', async t => {
  const requests = [];
  const server = http.createServer((request, response) => {
    requests.push([request.method, request.url]);
    response.writeHead(200, {'Content-Length': '123'});
    response.end();
  });
  t.after(() => new Promise(resolve => server.close(resolve)));
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  for (const ENV_NAME of ['dev', 'ci']) {
    const result = await resolveArtifact(server, {ENV_NAME});
    assert.deepEqual(JSON.parse(result.stdout), {'backend-app': {
      s3bucket: 'artifacts', s3key: 'whatsdone-app/selected-revision.zip'
    }});
  }
  assert.deepEqual(requests, [
    ['HEAD', '/artifacts/whatsdone-app/selected-revision.zip'],
    ['HEAD', '/artifacts/whatsdone-app/selected-revision.zip']
  ]);
});

test('missing artifact or required settings stop backend deployment', async t => {
  let requests = 0;
  const server = http.createServer((request, response) => {
    requests++;
    response.writeHead(404);
    response.end();
  });
  t.after(() => new Promise(resolve => server.close(resolve)));
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  await assert.rejects(resolveArtifact(server), error => {
    assert.equal(error.stdout, '');
    assert.match(error.stderr, /Cannot verify backend artifact.*selected-revision.zip/);
    return true;
  });
  for (const name of ['BUILD_NUMBER', 'ARTIFACT_BUCKET', 'ARTIFACT_BASE_PATH']) {
    await assert.rejects(resolveArtifact(server, {[name]: ''}), error => {
      assert.equal(error.stdout, '');
      assert.match(error.stderr, new RegExp(`Environment variable ${name} is not set`));
      return true;
    });
  }
  assert.equal(requests, 1);
});
