// The backend Lambda bundle is built and uploaded exactly once by the
// `build-artifacts` CI job (see modules/backend/package.json `publish-artifact`).
// Per-environment deploys do NOT rebuild it — they resolve the already
// published artifact and emit its location so the `api` module points every
// environment's Lambda at the same bytes (CI builds, prod promotes).
if (!process.env.BUILD_NUMBER) {
  throw new Error('Environment variable BUILD_NUMBER is not set')
}

const s3bucket = process.env.ARTIFACT_BUCKET
const s3key = `${process.env.ARTIFACT_BASE_PATH}/${process.env.BUILD_NUMBER}.zip`

console.log(JSON.stringify({ 'backend-app': { s3bucket, s3key } }))
