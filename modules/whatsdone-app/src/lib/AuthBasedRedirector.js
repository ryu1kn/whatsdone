
'use strict';

const ServiceLocator = require('./ServiceLocator');

/**
 * Redirect user according to their authentication status
 */
class AuthBasedRedirector {

  constructor() {
    this._sessionValidator = ServiceLocator.sessionValidator;
  }

  redirect(request, session) {
    const isValidSession = this._sessionValidator.validate(session);
    const redirectPath = this._getRedirectPath(request.path, isValidSession);
    if (!redirectPath) return null;
    return {
      statusCode: '303',
      headers: {Location: redirectPath}
    };
  }

  _getRedirectPath(path, isValidSession) {
    if (isValidSession) {
      return path === '/signin' ? '/' : null;
    }
    return path === '/signin' ? null : '/signin';
  }

}

module.exports = AuthBasedRedirector;
