
const SchemaBasedRedirectMiddleware = require('../../../src/server/express-middlewares/SchemaBasedRedirect');

describe('Server SchemaBasedRedirectMiddleware', () => {

  it('does nothing if a user is using secure protocol', () => {
    const middleware = new SchemaBasedRedirectMiddleware();
    const req = {
      protocol: 'https',
      hostname: 'HOSTNAME',
      url: '/URL_STRING'
    };
    return promisifyHandler(middleware, req).then(({res, next}) => {
      expect(res.redirect).to.have.been.not.called;
      expect(next).to.have.been.called;
    });
  });

  it('redirects a user to the https compatible page if they are using insecure protocol', () => {
    const middleware = new SchemaBasedRedirectMiddleware();
    const req = {
      protocol: 'http',
      hostname: 'HOSTNAME',
      url: '/URL_STRING'
    };
    return promisifyHandler(middleware, req).then(({res, next}) => {
      expect(res.redirect).to.have.been.calledWith('https://HOSTNAME/URL_STRING');
      expect(next).to.have.been.not.called;
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
