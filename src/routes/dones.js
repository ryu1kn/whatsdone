var express = require('express');
var router = express.Router();

var Dones = require('../models/Dones');

// XXX: Check db availability at the beginning of app.js
Dones.isAvailable()
.then((isAvailable) => {
  if (!isAvailable) {
    console.error('Cannot connect to the DB. Server doesn\'t start up.');
    process.exit(1);
  }
});

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

router.post('/dones.json', function(req, res) {
  Dones.write(req.body)
    .then(function (data) {
      res.setHeader('Content-Type', 'application/json');
      res.setHeader('Cache-Control', 'no-cache');
      res.send(JSON.stringify(data));
    })
    .catch((reason) => { error500(reason, res); })
    .done();
});

module.exports = router;
