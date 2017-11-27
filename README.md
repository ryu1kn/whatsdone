
[![Build Status](https://travis-ci.org/ryu1kn/whatsdone.svg?branch=master)](https://travis-ci.org/ryu1kn/whatsdone) [![Code Climate](https://codeclimate.com/github/ryu1kn/whatsdone/badges/gpa.svg)](https://codeclimate.com/github/ryu1kn/whatsdone)

# What's Done

Personal activity tracker. Take memos of what I (or we) have done today.

- infrastructure
  - ~~heroku + MongoDB~~
  - ~~heroku + DynamoDB~~
  - ~~AWS Elastic Beanstalk (deployed with `kumo`) + DynamoDB~~
  - AWS API Gateway + Lambda + DynamoDB (all deployed with `kumo`) ⬅️ Now here
- backend
  - Node.js v6.10 (deployed on AWS Lambda)
- frontend
  - ~~React + flux~~
  - React + Redux ⬅️ Now here

### Deploying entire system

If you want to deploy `prod` environment, you need to have `prod` config in modules then execute:

```sh
$ AWS_PROFILE=your-profile AWS_REGION=ap-southeast-2 ENV_NAME=prod ./deploy-system.sh
```

### Tools

* Migrate done data from mongodb to dynamodb (written in Go). See its [README](./tools/copy-done-table/README.md)
* What's Done API Client (written in Haskell). See its [README](./tools/api-client/README.md)
