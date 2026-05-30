
const {PassThrough} = require('stream');
const {S3Client} = require('@aws-sdk/client-s3');
const {Upload} = require('@aws-sdk/lib-storage');
const archiver = require('archiver');

const s3Client = new S3Client();

module.exports = ({directoryPath, s3bucket, s3key}) => {
  const state = {directoryPath, s3bucket, s3key};
  return Promise.resolve(state)
    .then(state => zipDirectory(state))
    .then(state => uploadZip(state))
    .then(dropReturnValue);
};

function zipDirectory(state) {
  const archive = archiver('zip');

  // @aws-sdk/lib-storage requires a native stream.Readable as the upload
  // Body, but archiver builds on the userland `readable-stream` package, so
  // its output fails lib-storage's `instanceof Readable` check. Bridge it
  // through a native PassThrough and forward archiver errors so a failed
  // zip still rejects the upload.
  const zipStream = new PassThrough();
  archive.on('error', err => zipStream.destroy(err));
  archive.pipe(zipStream);

  archive.directory(state.directoryPath, false);
  archive.finalize();
  return {...state, zipStream};
}

function uploadZip(state) {
  const upload = new Upload({
    client: s3Client,
    params: {
      Bucket: state.s3bucket,
      Key: state.s3key,
      Body: state.zipStream
    }
  });
  return upload.done();
}

function dropReturnValue() {}
