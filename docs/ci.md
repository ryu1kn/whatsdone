# Artifact deployment

Publish the backend once with `yarn --cwd modules/backend publish-artifact`.
Set `BUILD_NUMBER`, `ARTIFACT_BUCKET`, and `ARTIFACT_BASE_PATH` explicitly.
Use the existing backend artifact prefix, `whatsdone-app`.

Deploy the selected backend with `./deploy-module.sh --env <environment> backend api`.
Deployment resolves the published ZIP without rebuilding it. Keep published keys unchanged.
