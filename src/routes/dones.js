var _ = require('lodash');
var express = require('express');
var router = express.Router();

var Dones = require('../models/Dones');

function error500 (reason, res) {
  console.error(reason);
  res.status(500);
  res.send('500: Internal Server Error');
}

router.get('/', function(req, res) {
  Dones.read()
    .then(function (data) {
      res.setHeader('Content-Type', 'application/json');
      res.send(data);
    })
    .catch((reason) => { error500(reason, res); })
    .done();
});

router.post('/', function(req, res) {
  Dones.write(_.assign({}, req.body, {userId: req.session.userId}))
    .then(function (data) {
      res.setHeader('Content-Type', 'application/json');
      res.setHeader('Cache-Control', 'no-cache');
      res.send(JSON.stringify(data));
    })
    .catch((reason) => { error500(reason, res); })
    .done();
});

module.exports = router;
