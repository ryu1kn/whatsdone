# Artifact deployment

Publish the backend once with `yarn --cwd modules/backend publish-artifact`.
Set `BUILD_NUMBER`, `ARTIFACT_BUCKET`, and `ARTIFACT_BASE_PATH` explicitly.
Use the existing backend artifact prefix, `whatsdone-app`.

Deploy the selected backend with `./deploy-module.sh --env <environment> backend api`.
Deployment resolves the published ZIP without rebuilding it. Keep published keys unchanged. Record the Git revision, ZIP SHA-256, and selected environment settings with each deployment.

Publish web assets once with `yarn --cwd modules/web-assets publish-artifact`.
The existing web artifact prefix is `whatsdone-assets`.
Set `ENV_NAME` and reuse `BUILD_NUMBER` with `yarn --cwd modules/web-assets deploy:app`.
Deployment adds that environment's `appConfig.json` to the downloaded bundle.
`deploy-system.sh` selects published artifacts and never rebuilds them.
