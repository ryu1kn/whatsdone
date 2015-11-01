var _ = require('lodash');
var q = require('q');
var express = require('express');
var router = express.Router();

var Dones = require('../models/Dones');
var Users = require('../models/Users');

function errorResponse(reason, res) {
  // TODO: Instead of having a rule for error message format
  //       to destinguish error types, define custom exception classes
  var parsedInfo = reason.message.match(/^\[([^\]]+)]:(.*)/),
      errorKind = parsedInfo[1],
      logMessage = parsedInfo[2],
      clientMessage;

  switch (errorKind) {
    case 'AccessDeined':
      res.status(403);
      clientMessage = '403: Forbidden';
      break;

    case 'NotFound':
      res.status(404);
      clientMessage = '404: Not Found';
      break;

    default:
      res.status(500);
      clientMessage = '500: Internal Server Error';
  }
  console.error(logMessage);
  res.send(clientMessage);
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
    .catch((reason) => { errorResponse(reason, res); })
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
    .catch((reason) => { errorResponse(reason, res); })
    .done();
});

router.delete('/:id', function(req, res) {
  Dones.remove(req.params.id, req.session.userId)
    .then(() => {
      res.end();
    })
    .catch((reason) => {
      errorResponse(reason, res);
    })
    .done();
});

module.exports = router;
