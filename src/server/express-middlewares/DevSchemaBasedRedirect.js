
/**
 * Do nothing even if a user is using insecure protocol
 */
class SchemaBasedRedirectMiddleware {

  handle(req, res, next) {
    next();
  }

}

module.exports = SchemaBasedRedirectMiddleware;
