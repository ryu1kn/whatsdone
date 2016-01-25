'use strict';

let express = require('express');
let router = express.Router();

/* GET home page. */
router.get('/', function(req, res, _next) {
  res.render('index', { title: 'What\'s done?' });
});

module.exports = router;
