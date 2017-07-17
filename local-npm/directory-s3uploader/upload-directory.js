
const AWS = require('aws-sdk');
const archiver = require('archiver');
const s3Stream = require('s3-upload-stream')(new AWS.S3());

module.exports = ({directoryPath, s3bucket, s3key}) => {
  const state = {directoryPath, s3bucket, s3key};
  return Promise.resolve(state)
    .then(state => zipDirectory(state))
    .then(state => uploadZip(state))
    .then(dropReturnValue);
};

function zipDirectory(state) {
  const archive = archiver('zip', {store: true});
  archive.directory(state.directoryPath, false);
  archive.finalize();
  return Object.assign({}, state, {zipStream: archive});
}

function uploadZip(state) {
  const params = {
    Bucket: state.s3bucket,
    Key: state.s3key
  };
  return new Promise((resolve, reject) => {
    pipeStreams([state.zipStream, s3Stream.upload(params)], reject)
      .on('finish', resolve);
  });
}

function pipeStreams(streams, reject) {
  streams.forEach(stream => {
    stream.on('error', reject);
  });
  const [firstStream, ...restStreams] = streams;
  return restStreams.reduce(
    (joinedStream, stream) => joinedStream.pipe(stream),
    firstStream
  );
}

function dropReturnValue() {}
