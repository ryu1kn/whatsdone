var express = require('express');
var router = express.Router();

var Users = require('../models/Users');

router.get('/', function (req, res) {
  res.render('signin', {
    title: 'Sign In - What\'s done?'
  });
});

router.post('/', function (req, res, next) {
  Users.findUser(req.body)
    .then((user) => {
      if (user) {
        req.session.isAuthorized = true;
        req.session.userId = user._id;
        res.redirect('/');
      } else {
        res.status(401);
        res.render('signin');
      }
    })
    .catch(reason => { next(reason); })
    .done();
});

module.exports = router;
