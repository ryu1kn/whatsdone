
/**
 * Do nothing even if a user is using insecure protocol
 */
class SchemaBasedRedirectMiddleware {

  handle(req, res, next) {
    console.log('Connecting protocol @DevSchemaBasedRedirectMiddleware:', req.protocol);
    next();
  }

}

module.exports = SchemaBasedRedirectMiddleware;
