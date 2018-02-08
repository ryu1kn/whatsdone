const uploadDirectory = require('directory-s3uploader')

if (!process.env.BUILD_NUMBER) {
  throw new Error('Environment variable BUILD_NUMBER is not set')
}

const uploadLocation = {
  s3bucket: process.env.ARTIFACT_BUCKET,
  s3key: `${process.env.ARTIFACT_BASE_PATH}/${process.env.BUILD_NUMBER}.zip`
}

const params = Object.assign({}, uploadLocation, {
  directoryPath: process.env.npm_package_config_BUILD_DIR
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
