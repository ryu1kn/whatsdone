
'use strict';

class SignoutRequestHandler {

  handle(req, res) {
    delete req.session.isAuthorized;
    res.redirect('/signin');
  }

}

module.exports = SignoutRequestHandler;
