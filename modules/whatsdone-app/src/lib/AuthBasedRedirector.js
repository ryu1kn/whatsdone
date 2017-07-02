
const ServiceLocator = require('./ServiceLocator');

/**
 * TODO: Rename the class as it no longer redirects
 */
class AuthBasedRedirector {

  constructor() {
    this._sessionValidator = ServiceLocator.sessionValidator;
  }

  redirect(request, session) {
    const isValidSession = this._sessionValidator.validate(session);
    if (isValidSession || request.path === '/signin') return null;
    return {
      statusCode: '401'
    };
  }

}

module.exports = AuthBasedRedirector;
