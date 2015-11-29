'use strict';

let express = require('express');
let router = express.Router();

function signout(req, res) {
  delete req.session.isAuthorized;
  res.redirect('/signin');
}

router.get('/', signout);
router.post('/', signout);

module.exports = router;
