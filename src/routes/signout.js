var express = require('express');
var router = express.Router();

function signout(req, res) {
  delete req.session.isAuthorized;
  res.redirect('/signin');
}

router.get('/', signout);
router.post('/', signout);

module.exports = router;
