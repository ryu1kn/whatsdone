
[![Build Status](https://travis-ci.org/ryu1kn/whatsdone.svg?branch=master)](https://travis-ci.org/ryu1kn/whatsdone) [![Code Climate](https://codeclimate.com/github/ryu1kn/whatsdone/badges/gpa.svg)](https://codeclimate.com/github/ryu1kn/whatsdone)

# What's Done

Personal activity tracker. Take memos of what I (or we) have done today.

- infrastructure
  - ~~heroku + MongoDB~~
  - ~~heroku + DynamoDB~~
  - AWS Elastic Beanstalk (deployed by `kumo`) + DynamoDB
- backend
  - Node.js
- frontend
  - React + flux

## Notes

### How to use locally

Set environment variables (`AWS_PROFILE`, `SESSION_SECRET` and `DB_REGION`) and execute commands:

```sh
$ npm install
$ npm start
```

Access to `http://localhost:3000` with your web browser.

### Deploy to AWS Elasticbeanstalk

* Just do `eb deploy`

### Tools

* Migrate done data from mongodb to dynamodb
