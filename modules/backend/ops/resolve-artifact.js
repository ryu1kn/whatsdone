const {S3Client, HeadObjectCommand} = require('@aws-sdk/client-s3')

// Resolve the published ZIP without rebuilding or uploading it.
for (const name of ['BUILD_NUMBER', 'ARTIFACT_BUCKET', 'ARTIFACT_BASE_PATH']) {
  if (!process.env[name]) {
    throw new Error(`Environment variable ${name} is not set`)
  }
}

const s3bucket = process.env.ARTIFACT_BUCKET
const s3key = `${process.env.ARTIFACT_BASE_PATH}/${process.env.BUILD_NUMBER}.zip`

const s3Client = new S3Client({region: process.env.AWS_REGION})

s3Client.send(new HeadObjectCommand({Bucket: s3bucket, Key: s3key}))
  .then(() => {
    console.log(JSON.stringify({'backend-app': {s3bucket, s3key}}))
  })
  .catch(e => {
    console.error(
      `Cannot verify backend artifact at s3://${s3bucket}/${s3key}. ` +
      `Publish it before deployment; verify access and artifact settings. (${e.name})`
    )
    process.exitCode = 1
  })
