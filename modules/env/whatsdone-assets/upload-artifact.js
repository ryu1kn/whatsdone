
const uploadDirectory = require('directory-s3uploader');

if (!process.env.BUILD_NUMBER) {
  throw new Error('Environment variable BUILD_NUMBER is not set');
}

const config = require(`./module-config/${process.env.ENV_NAME}`);

const params = {
  directoryPath: process.env.npm_package_config_APP_BUILD_DIR,
  s3bucket: config.artifactBucket,
  s3Key: `${config.artifactBasePath}/${process.env.BUILD_NUMBER}.zip`
};
uploadDirectory(params)
  .catch(e => {
    setTimeout(() => {
      throw e;
    }, 0);
  });
