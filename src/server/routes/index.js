'use strict';

let express = require('express');
let router = express.Router();  // eslint-disable-line new-cap

/* GET home page. */
router.get('/', function (req, res, _next) {
  res.render('index', {title: 'What\'s done?'});
});

module.exports = router;
