const assert = require('node:assert/strict');
const {execFile, execFileSync} = require('node:child_process');
const fs = require('node:fs/promises');
const http = require('node:http');
const os = require('node:os');
const path = require('node:path');
const {promisify} = require('node:util');
const {test} = require('node:test');

const run = promisify(execFile);

function upload(directory, server) {
  return run(process.execPath, ['-e',
    'require(process.env.UPLOADER_MODULE || "directory-s3uploader")({' +
    'directoryPath: process.argv[1], s3bucket: "artifacts", s3key: "123.zip"})' +
    '.catch(error => { console.error(error); process.exitCode = 1; });', directory], {
    cwd: __dirname,
    timeout: 10000,
    env: {...process.env,
      AWS_ACCESS_KEY_ID: 'test', AWS_SECRET_ACCESS_KEY: 'test',
      AWS_SESSION_TOKEN: '', AWS_PROFILE: '', AWS_REGION: 'ap-southeast-2',
      AWS_ENDPOINT_URL_S3: `http://127.0.0.1:${server.address().port}`,
      AWS_REQUEST_CHECKSUM_CALCULATION: 'WHEN_REQUIRED', AWS_MAX_ATTEMPTS: '1'
    }
  });
}

test('uploaded ZIP preserves nested text and binary files', async t => {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), 'artifact-upload-'));
  t.after(() => fs.rm(directory, {recursive: true, force: true}));
  await fs.mkdir(path.join(directory, 'input', 'nested'), {recursive: true});
  await fs.writeFile(path.join(directory, 'input', 'nested', 'memo.txt'), 'DONE: café\n');
  await fs.writeFile(path.join(directory, 'input', 'bytes.bin'), Buffer.from([0, 255, 10, 128]));

  let uploaded;
  const server = http.createServer(async (request, response) => {
    const chunks = [];
    for await (const chunk of request) {
      chunks.push(chunk);
    }
    uploaded = Buffer.concat(chunks);
    response.writeHead(200, {ETag: '"uploaded"'});
    response.end();
  });
  t.after(() => new Promise(resolve => server.close(resolve)));
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));

  await upload(path.join(directory, 'input'), server);

  const archive = path.join(directory, 'artifact.zip');
  await fs.writeFile(archive, uploaded);
  assert.equal(execFileSync('unzip', ['-p', archive, 'nested/memo.txt']).toString(), 'DONE: café\n');
  assert.deepEqual(execFileSync('unzip', ['-p', archive, 'bytes.bin']), Buffer.from([0, 255, 10, 128]));
});

test('upload rejects with the S3 error when access is denied', async t => {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), 'artifact-upload-denied-'));
  t.after(() => fs.rm(directory, {recursive: true, force: true}));
  await fs.writeFile(path.join(directory, 'memo.txt'), 'DONE');
  const server = http.createServer((request, response) => {
    request.resume();
    response.writeHead(403, {'Content-Type': 'application/xml'});
    response.end('<Error><Code>AccessDenied</Code><Message>Upload denied</Message></Error>');
  });
  t.after(() => new Promise(resolve => server.close(resolve)));
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  await assert.rejects(upload(directory, server), error => {
    assert.match(error.stderr, /AccessDenied: Upload denied/);
    return true;
  });
});
