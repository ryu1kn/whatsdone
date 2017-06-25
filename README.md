
[![Build Status](https://travis-ci.org/ryu1kn/whatsdone.svg?branch=master)](https://travis-ci.org/ryu1kn/whatsdone) [![Code Climate](https://codeclimate.com/github/ryu1kn/whatsdone/badges/gpa.svg)](https://codeclimate.com/github/ryu1kn/whatsdone)

# What's Done

Personal activity tracker. Take memos of what I (or we) have done today.

- infrastructure
  - ~~heroku + MongoDB~~
  - ~~heroku + DynamoDB~~
  - ~~AWS Elastic Beanstalk (deployed with `kumo`) + DynamoDB~~
  - API Gateway + Lambda + DynamoDB (all deployed with `kumo`)
- backend
  - Node.js
- frontend
  - React + flux

### Deploying module

For example, if you want to deploy whatsdone-api module, execute following:

```sh
$ AWS_PROFILE=your-profile ./deploy-module.sh -m whatsdone-api -- --env prod --region ap-southeast-2
```

### Tools

* Migrate done data from mongodb to dynamodb (written in Go). See its [README](./tools/copy-done-table/README.md)
