'use strict';

let express = require('express');
let router = express.Router();  // eslint-disable-line new-cap

function signout(req, res) {
  delete req.session.isAuthorized;
  res.redirect('/signin');
}

router.get('/', signout);
router.post('/', signout);

module.exports = router;
