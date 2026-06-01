const uploadDirectory = require('directory-s3uploader')
const {join} = require('path')

for (const name of ['BUILD_NUMBER', 'ARTIFACT_BUCKET', 'ARTIFACT_BASE_PATH']) {
  if (!process.env[name]) {
    throw new Error(`Environment variable ${name} is not set`)
  }
}

const uploadLocation = {
  s3bucket: process.env.ARTIFACT_BUCKET,
  s3key: `${process.env.ARTIFACT_BASE_PATH}/${process.env.BUILD_NUMBER}.zip`
}

const params = Object.assign({}, uploadLocation, {
  directoryPath: join(__dirname, '..', process.env.npm_package_config_BUILD_DIR_NAME)
})
uploadDirectory(params)
  .then(() => {
    console.log(JSON.stringify({ 'backend-app': uploadLocation }))
  })
  .catch(e => {
    setTimeout(() => {
      throw e
    }, 0)
  })
