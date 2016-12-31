
const SECURE_PROTOCOL = 'https';

/**
 * Redirect a user if they're not using secure protocol
 */
class SchemaBasedRedirectMiddleware {

  handle(req, res, next) {
    if (req.protocol === SECURE_PROTOCOL) next();
    else res.redirect(`${SECURE_PROTOCOL}://${req.hostname}${req.url}`);
  }

}

module.exports = SchemaBasedRedirectMiddleware;
