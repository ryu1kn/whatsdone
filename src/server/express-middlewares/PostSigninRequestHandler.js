
const ServiceLocator = require('../ServiceLocator');

class PostSigninRequestHandler {

  constructor() {
    this._userRepository = ServiceLocator.userRepository;
  }

  handle(req, res, next) {
    this._userRepository.findUser(req.body)
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
      .catch(next);
  }

}

module.exports = PostSigninRequestHandler;
