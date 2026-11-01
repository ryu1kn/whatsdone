
# End-to-end test

## How to run/debug

```sh
export AWS_PROFILE=<your-profile>
export AWS_REGION=<your-region>

DEBUG=pw:api PWDEBUG=1 make test
```

For local checks, start the mock API and frontend in separate terminals:

```sh
cd modules/web-assets/app
yarn start:api
# Separate terminal, same directory:
yarn start:dev
# From repository root:
yarn --cwd test/e2e install
yarn --cwd test/e2e playwright install chromium
node test/e2e/test.js
```

Checks create/edit/delete persistence, special characters, layout overflow, and browser errors.
Local runs require the mock API; `ENV_NAME=ci` retains hosted authentication.

## References

* [Playwright](https://playwright.dev/docs/intro)
* [Playwright-cli](https://www.npmjs.com/package/playwright-cli)
