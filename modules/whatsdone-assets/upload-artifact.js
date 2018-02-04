const uploadDirectory = require('directory-s3uploader');

if (!process.env.BUILD_NUMBER) {
  throw new Error('Environment variable BUILD_NUMBER is not set');
}

const {artifactBucket, artifactBasePath} = require('./module-config/common');

const params = {
  directoryPath: process.env.npm_package_config_APP_BUILD_DIR,
  s3bucket: artifactBucket,
  s3key: `${artifactBasePath}/${process.env.BUILD_NUMBER}.zip`
};
uploadDirectory(params).catch(e => {
  setTimeout(() => {
    throw e;
  }, 0);
});
