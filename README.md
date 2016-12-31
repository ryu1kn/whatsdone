
[![Build Status](https://travis-ci.org/ryu1kn/whatsdone.svg?branch=master)](https://travis-ci.org/ryu1kn/whatsdone) [![Code Climate](https://codeclimate.com/github/ryu1kn/whatsdone/badges/gpa.svg)](https://codeclimate.com/github/ryu1kn/whatsdone)

# What's Done

Personal activity tracker. Take memos of what I (or we) have done today.

- server: heroku
- backend: Node.js
- frontend: React

## Notes

### How to use locally

Set environment variables (`AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `SESSION_SECRET` and `DB_REGION`) and execute commands:

```sh
$ npm install
$ npm start
```

Access to `http://localhost:3000` with your web browser.

### Deploy to Heroku

* Just do `git push heroku master`
