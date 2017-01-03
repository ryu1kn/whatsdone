
const DevSchemaBasedRedirectMiddleware = require('../../../src/server/express-middlewares/DevSchemaBasedRedirect');

describe('Server DevSchemaBasedRedirectMiddleware', () => {

  it('does nothing even if a user is using insecure protocol', () => {
    const middleware = new DevSchemaBasedRedirectMiddleware();
    const req = {protocol: 'http'};
    return promisifyExpressMiddleware(middleware, req).then(({res, next}) => {
      expect(res.redirect).to.have.been.not.called;
      expect(next).to.have.been.called;
    });
  });

});
