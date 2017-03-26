
'use strict';

/**
 * Redirect user according to their authentication status
 */
class AuthBasedRedirector {

  redirect(request) {
    const redirectPath = this._getRedirectPath(request.path, request.session.isAuthorized);
    if (!redirectPath) return null;
    return {
      statusCode: '303',
      headers: {Location: redirectPath}
    };
  }

  _getRedirectPath(path, isAuthorized) {
    if (isAuthorized) {
      return path === '/signin' ? '/' : null;
    }
    return path === '/signin' ? null : '/signin';
  }

}

module.exports = AuthBasedRedirector;