
'use strict';

/**
 * Do nothing even if a user is using insecure protocol
 */
class HttpSchemeBasedRedirectMiddleware {

  handle(req, res, next) {
    next();
  }

}

module.exports = HttpSchemeBasedRedirectMiddleware;
