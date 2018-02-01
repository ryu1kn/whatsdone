
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
  return pipeStreams(state.zipStream, s3Stream.upload(params));
}

function pipeStreams(...streams) {
  return new Promise((resolve, reject) => {
    streams.forEach(stream => {
      stream.on('error', errorMessage => {
        reject(new Error(errorMessage));
      });
    });

    const [firstStream, ...restStreams] = streams;
    const pipedStream = restStreams.reduce(
      (joinedStream, stream) => joinedStream.pipe(stream),
      firstStream
    );
    pipedStream.on('finish', resolve);
  });
}

function dropReturnValue() {}
