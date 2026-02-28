# REQ-2: PR workflow failure reproduction

This note captures a local reproduction path for why the PR workflow in `.github/workflows/whatsdone.yml` is unreliable and can fail.

## Reproduction commands

1. Confirm PR trigger and missing job-level guards:

```bash
rg -n "^on:|pull_request|ci-deploy:|ci-e2e-test:|production-deploy:|^\s+if:" .github/workflows/whatsdone.yml
```

Expected signal:
- `pull_request` trigger exists.
- `ci-deploy`, `ci-e2e-test`, and `production-deploy` jobs exist.
- No job-level `if:` guard is present to exclude PR events.

2. Emulate PR branch behavior in deploy step:

```bash
BUILD_BRANCH='123/merge' ./ci-build.sh
```

Observed output:

```text
Deployment can happen only from "main" branch
```

This exits successfully without deploying artifacts.

3. Reproduce e2e credential fetch behavior used by CI:

```bash
pw="$(AWS_REGION=ap-southeast-2 make --no-print-directory -C test/e2e get-password 2>/tmp/req2_get_password_err.log)"
printf 'PASSWORD_LEN:%s\n' "${#pw}"
printf 'MAKE_EXIT:%s\n' "$?"
sed -n '1,3p' /tmp/req2_get_password_err.log
```

Observed output in this environment:

```text
PASSWORD_LEN:0
MAKE_EXIT:0
Could not connect to the endpoint URL: "https://secretsmanager.ap-southeast-2.amazonaws.com/"
```

## Root-cause hypothesis

- The workflow runs deploy/e2e/prod jobs for `pull_request` events, but deploy behavior in `ci-build.sh` is branch-gated and no-ops for non-`main` refs.
- Because downstream jobs still run, the e2e step can execute without a fresh deployment and with brittle secret retrieval behavior (`get-password` can yield an empty password while returning success due pipeline exit-code masking).
- This mismatch is a concrete failure path explaining why PR runs do not reliably reach all-green state.

## Local command sequence summary

Use this sequence to reproduce the relevant failure path quickly:

```bash
rg -n "^on:|pull_request|ci-deploy:|ci-e2e-test:|production-deploy:|^\s+if:" .github/workflows/whatsdone.yml
BUILD_BRANCH='123/merge' ./ci-build.sh
pw="$(AWS_REGION=ap-southeast-2 make --no-print-directory -C test/e2e get-password 2>/tmp/req2_get_password_err.log)"; echo "len=${#pw}"; sed -n '1,3p' /tmp/req2_get_password_err.log
```
