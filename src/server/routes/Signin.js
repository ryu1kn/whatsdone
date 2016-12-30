'use strict';

let express = require('express');
let router = express.Router();  // eslint-disable-line new-cap

const ServiceLocator = require('../ServiceLocator');
const userRepository = ServiceLocator.userRepository;

router.get('/', function (req, res) {
  res.render('signin', {
    title: 'Sign In - What\'s done?'
  });
});

router.post('/', function (req, res, next) {
  userRepository.findUser(req.body)
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
    .catch(reason => { next(reason); });
});

module.exports = router;
