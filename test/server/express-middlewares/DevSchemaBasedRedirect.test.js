
const DevSchemaBasedRedirectMiddleware = require('../../../src/server/express-middlewares/DevSchemaBasedRedirect');

describe('Server DevSchemaBasedRedirectMiddleware', () => {

  it('does nothing even if a user is using insecure protocol', () => {
    const middleware = new DevSchemaBasedRedirectMiddleware();
    const req = {protocol: 'http'};
    return promisifyHandler(middleware, req).then(({res, next}) => {
      expect(res.redirect).to.have.been.not.called;
      expect(next).to.have.been.called;
    });
  });

});

function promisifyHandler(middleware, req) {
  return new Promise((resolve, reject) => {
    const result = {
      res: {redirect: sinon.spy()},
      next: sinon.spy()
    };
    const next = (...args) => {
      result.next(...args);
      resolve(result);
    };
    const res = {
      redirect: (...args) => {
        result.res.redirect(...args);
        resolve(result);
      }
    };
    try {
      middleware.handle(req, res, next);
    } catch (e) {
      reject(e);
    }
  });

}
