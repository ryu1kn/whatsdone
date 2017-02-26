
const DevSchemaBasedRedirectMiddleware = require('../../../src/server/express-middlewares/DevSchemaBasedRedirect');

describe('Server DevSchemaBasedRedirectMiddleware', () => {

  it('does nothing even if a user is using insecure protocol', () => {
    const middleware = new DevSchemaBasedRedirectMiddleware();
    const req = {protocol: 'http'};
    return promisifyExpressMiddleware(middleware, req).then(result => {
      expect(result.res.redirect).to.have.been.not.called;
      expect(result.next).to.have.been.called;
    });
  });

});
