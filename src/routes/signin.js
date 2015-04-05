var express = require('express');
var router = express.Router();

var Users = require('../models/Users');

router.get('/', function (req, res) {
  res.render('signin', {
    title: 'Sign In - What\'s done?'
  });
});

router.post('/', function (req, res) {
  Users.authenticate(req.body)
    .then(function (isAuthorized) {
      if (isAuthorized) {
        req.session.isAuthorized = true;
        res.redirect('/');
      } else {
        res.status(401);
        res.render('signin');
      }
    })
    .done();
});

module.exports = router;
