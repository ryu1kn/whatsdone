const {S3Client, HeadObjectCommand} = require('@aws-sdk/client-s3')

// The backend Lambda bundle is built and uploaded exactly once by the
// `build-artifacts` CI job (see modules/backend/package.json `publish-artifact`).
// Per-environment deploys do NOT rebuild it — this script verifies the already
// published artifact exists and emits its location so the `api` module points
// every environment's Lambda at the same bytes (CI builds, prod promotes).
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
      `Backend artifact not found at s3://${s3bucket}/${s3key}. ` +
      `It must be published by the build-artifacts job before deploy. (${e.name})`
    )
    process.exitCode = 1
  })
