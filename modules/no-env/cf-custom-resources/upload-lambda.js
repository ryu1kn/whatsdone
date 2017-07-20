
const uploadDirectory = require('directory-s3uploader');
const config = require('./module-config');

const params = {
  directoryPath: process.env.npm_package_config_SRC_DIR,
  s3bucket: config.artifactBucket,
  s3key: `cf-custom-resources/${process.env.BUILD_NUMBER}.zip`
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
