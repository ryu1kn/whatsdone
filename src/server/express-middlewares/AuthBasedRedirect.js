
/**
 * Redirect user according to their authentication status
 */
class AuthBasedRedirectMiddleware {

  handle(req, res, next) {
    const redirectPath = this._getRedirectPath(req.path, req.session.isAuthorized);
    if (redirectPath) res.redirect(redirectPath);
    else next();
  }

  _getRedirectPath(path, isAuthorized) {
    if (isAuthorized) {
      return path === '/signin' ? '/' : null;
    }
    return path === '/signin' ? null : '/signin';
  }

}

module.exports = AuthBasedRedirectMiddleware;
