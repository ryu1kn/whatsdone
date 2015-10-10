var _ = require('lodash');
var q = require('q');
var express = require('express');
var router = express.Router();

var Dones = require('../models/Dones');
var Users = require('../models/Users');

function error500(reason, res) {
  console.error(reason);
  res.status(500);
  res.send('500: Internal Server Error');
}

function setUserName(done) {
  return q(done.userId ? Users.getById(done.userId) : {name: null})
      .then((user) => _.assign(done, {username: user.name}));
}

function setUserNames(dones) {
  return Users.getByIds(_.pluck(dones, 'userId'))
    .then((users) => {
      var nameMap = _.indexBy(users, '_id');
      return dones.map((done) => {
        if (done.userId) {
          done.username = nameMap[done.userId].name;
        }
        return done;
      });
    });
}

router.get('/', function(req, res) {
  Dones.read()
    .then((dones) => setUserNames(dones))
    .then((dones) => {
      res.setHeader('Content-Type', 'application/json');
      res.send(dones);
    })
    .catch((reason) => { error500(reason, res); })
    .done();
});

router.post('/', function(req, res) {
  Dones.write(_.assign({}, req.body, {userId: req.session.userId}))
    .then((done) => setUserName(done))
    .then((done) => {
      res.setHeader('Content-Type', 'application/json');
      res.setHeader('Cache-Control', 'no-cache');
      res.send(JSON.stringify(done));
    })
    .catch((reason) => { error500(reason, res); })
    .done();
});

router.delete('/', function(req, res) {
  Dones.remove(req.query.id)
    .then(() => {
      res.end();
    })
    .catch((reason) => { error500(reason, res); })
    .done();
});

module.exports = router;
