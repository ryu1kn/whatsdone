var _ = require('lodash');
var express = require('express');
var router = express.Router();

var q = require('q');

var Dones = require('../models/Dones');
var Users = require('../models/Users');

function error500(reason, res) {
  console.error(reason);
  res.status(500);
  res.send('500: Internal Server Error');
}

function setUserName(dones) {
  return q.all(dones.map((done) =>
    q(done.userId ? Users.getById(done.userId) : {name: null})
      .then((user) => _.assign(done, {username: user.name}))
  ));
}

router.get('/', function(req, res) {
  Dones.read()
    .then((dones) => setUserName(dones))
    .then((dones) => {
      res.setHeader('Content-Type', 'application/json');
      res.send(dones);
    })
    .catch((reason) => { error500(reason, res); })
    .done();
});

router.post('/', function(req, res) {
  Dones.write(_.assign({}, req.body, {userId: req.session.userId}))
    .then((dones) => setUserName(dones))
    .then((dones) => {
      res.setHeader('Content-Type', 'application/json');
      res.setHeader('Cache-Control', 'no-cache');
      res.send(JSON.stringify(dones));
    })
    .catch((reason) => { error500(reason, res); })
    .done();
});

module.exports = router;
