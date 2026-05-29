# What's Done

This is a personal activity tracker project that allows users to take memos of what they have done. Each task done is called "DONE" in this project, it is an important term in this project. It's a full-stack application built with TypeScript, React, and AWS.

## Project Overview

The project is a monolithic repository containing the frontend and backend code. The backend is built with Node.js and runs on AWS Lambda, while the frontend is a React application. The project uses AWS services like API Gateway, DynamoDB, Cognito, and S3 for its infrastructure.

## Additional rules

Pre-defined additional rules can be provided by a developer from [prompts](/prompts) directory on-demand.
When requested, please update the rules to keep refining the rules.

## Workspace

You're free to save your plans / tools under [`workspace`](/workspace) directory. For this you have `plans` and `tools` directories already created.
Ad-hoc scripts and tools created during development, such as those for data generation or transformation, should be retained within the `workspace/tools` directory (or a relevant module's `tools` directory) for traceability and potential future use. These files serve as valuable artifacts documenting the development process.

## Building and Running

The project uses `yarn` for dependency management. To build the project, you can use the following commands:

* **Backend:**

  ```bash
  cd modules/backend
  yarn install
  yarn build
  ```

* **Frontend:**

  ```bash
  cd modules/web-assets
  yarn install
  yarn build
  ```

### Testing

The project has unit tests for both the backend and frontend. To run the tests, you can use the following commands:

* **Backend:**

  ```bash
  cd modules/backend
  yarn test
  ```

* **Frontend:**

  ```bash
  cd modules/web-assets/app
  yarn test
  ```

### Deployment

The project is deployed on AWS. The deployment is automated using GitHub Actions. The workflow is defined in `.github/workflows/build-and-deploy.yml`.

The workflow runs different jobs depending on the trigger:

* **Pull request**: the `pr-check` job runs `mise run gate` (cfn-lint, Checkov, and module unit tests). It does not touch AWS or deploy anything.
* **Manual run (`workflow_dispatch`)**: the `oidc-test-ci` job verifies the CI OIDC connection.
* **Push to `main`**: the deploy pipeline runs:

  1.  The `ci-deploy` job is triggered. It lints and scans the CloudFormation templates (`cfn-lint`, Checkov), then runs the `./ci-build.sh` script with `ENV_NAME=ci`.
  2.  The `ci-build.sh` script uses `buildmate` to build and deploy only the modules that have changed.
  3.  After `ci-deploy` completes, the `ci-e2e-test` job runs the end-to-end tests against the CI environment.
  4.  If the end-to-end tests pass, the `production-deploy` job runs the `./ci-build.sh` script with `ENV_NAME=prod` to deploy the changes to the production environment.

## Development Conventions

The project uses TypeScript for both the backend and frontend. It also uses ESLint for linting. The code is well-structured and follows the standard conventions for a full-stack TypeScript application.
