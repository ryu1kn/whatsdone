'use strict';

let express = require('express');
let router = express.Router();  // eslint-disable-line new-cap

let Users = require('../models/Users');

router.get('/', function (req, res) {
  res.render('signin', {
    title: 'Sign In - What\'s done?'
  });
});

router.post('/', function (req, res, next) {
  Users.findUser(req.body)
    .then(user => {
      if (user) {
        req.session.isAuthorized = true;
        req.session.userId = user.id;
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
