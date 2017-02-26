
'use strict';

const SECURE_PROTOCOL = 'https';

/**
 * Redirect a user if they're not using secure protocol
 */
class HttpSchemeBasedRedirectMiddleware {

  // Heroku uses `X-Forwarded-Proto` to tell you the original protocol.
  // See http://stackoverflow.com/questions/7185074/heroku-nodejs-http-to-https-ssl-forced-redirect
  handle(req, res, next) {
    if (req.header('X-Forwarded-Proto') === SECURE_PROTOCOL) next();
    else res.redirect(`${SECURE_PROTOCOL}://${req.hostname}${req.url}`);
  }

}

module.exports = HttpSchemeBasedRedirectMiddleware;
