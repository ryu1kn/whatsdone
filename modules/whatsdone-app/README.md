# What's Done backend

## TODOs

* [X] Rewrite JavaScript with TypeScript
* [X] Replacing [Sinon.JS](https://github.com/sinonjs/sinon) with [testdouble.js](https://github.com/testdouble/testdouble.js/)
* [ ] Reduce mocks in unit tests

## Locally running topic classifier

```sh
ENV_NAME=prod IS_LOCAL_RUN=true yarn ts-node classify-topic.ts "Test topic classification from CLI"
```
