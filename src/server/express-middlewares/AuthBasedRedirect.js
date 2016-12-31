
/**
 * Redirect user according to their authentication status
 */
class AuthBasedRedirectMiddleware {

  handle(req, res, next) {
    if (req.path === '/signin') {
      if (req.session.isAuthorized) {
        res.redirect('/');
      } else {
        next();
      }
    } else if (req.session.isAuthorized) {
      next();
    } else {
      res.redirect('/signin');
    }
  }

}

module.exports = AuthBasedRedirectMiddleware;
