
const uploadDirectory = require('directory-s3uploader');
const argv = require('minimist')(process.argv.slice(2));

const params = {
  directoryPath: argv['target-dir'],
  s3bucket: argv.s3bucket,
  s3key: argv.s3key
};
uploadDirectory(params)
  .then(() => outputUploadLocation(params))
  .catch(e => {
    setTimeout(() => {
      throw e;
    }, 0);
  });

function outputUploadLocation(params) {
  const location = {
    S3Bucket: params.s3bucket,
    S3Key: params.s3key
  };
  console.log(JSON.stringify(location));
}
