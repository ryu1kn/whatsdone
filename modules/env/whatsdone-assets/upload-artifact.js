
const AWS = require('aws-sdk');
const archiver = require('archiver');
const s3Stream = require('s3-upload-stream')(new AWS.S3());

if (!process.env.BUILD_NUMBER) {
  throw new Error('Environment variable BUILD_NUMBER is not set');
}

const config = require(`./module-config/${process.env.ENV_NAME}`);
const BUILD_DIR = process.env.npm_package_config_APP_BUILD_DIR;

const state = {
  artifactBucketName: config.artifactBucket,
  artifactBasePath: config.artifactBasePath,
  artifactName: `${process.env.BUILD_NUMBER}.zip`
};
Promise.resolve(state)
  .then(state => zipArtifact(state))
  .then(state => uploadArtifact(state))
  .catch(e => {
    setTimeout(() => {
      throw e;
    }, 0);
  });

function zipArtifact(state) {
  const archive = archiver('zip', {store: true});
  archive.directory(BUILD_DIR, false);
  archive.finalize();
  return Object.assign({}, state, {zipStream: archive});
}

function uploadArtifact(state) {
  const params = {
    Bucket: state.artifactBucketName,
    Key: `${state.artifactBasePath}/${state.artifactName}`
  };
  console.log('Uploading to: ', params);
  return new Promise((resolve, reject) => {
    pipeStreams(state.zipStream, s3Stream.upload(params))
      .on('finish', resolve)
      .on('error', reject);
  });
}

function pipeStreams(...streams) {
  streams.forEach(stream => {
    stream.on('error', e => {
      if (e instanceof Error) console.error(e.stack);
      else console.error(e);
    });
  });
  const [firstStream, ...restStreams] = streams;
  return restStreams.reduce(
    (joinedStream, stream) => joinedStream.pipe(stream),
    firstStream
  );
}
