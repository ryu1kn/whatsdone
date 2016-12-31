
const AuthBasedRedirectMiddleware = require('../../../src/server/express-middlewares/AuthBasedRedirect');

describe('Server AuthBasedRedirectMiddlware', () => {

  it('does nothing if an authenticated user accesses any path', () => {
    const authBasedRedirectMiddleware = new AuthBasedRedirectMiddleware();
    const req = {
      path: '/PATH',
      session: {isAuthorized: true}
    };
    return promisifyHandler(authBasedRedirectMiddleware, req).then(({res, next}) => {
      expect(res.redirect).to.have.been.not.called;
      expect(next).to.have.been.called;
    });
  });

  it('does nothing if a non-authenticated user accesses signin page', () => {
    const authBasedRedirectMiddleware = new AuthBasedRedirectMiddleware();
    const req = {
      path: '/signin',
      session: {isAuthorized: false}
    };
    return promisifyHandler(authBasedRedirectMiddleware, req).then(({res, next}) => {
      expect(res.redirect).to.have.been.not.called;
      expect(next).to.have.been.called;
    });
  });

  it('redirects an authenticated user to the root page if they access to the signin page', () => {
    const authBasedRedirectMiddleware = new AuthBasedRedirectMiddleware();
    const req = {
      path: '/signin',
      session: {isAuthorized: true}
    };
    return promisifyHandler(authBasedRedirectMiddleware, req).then(({res, next}) => {
      expect(res.redirect).to.have.been.calledWith('/');
      expect(next).to.have.been.not.called;
    });
  });

  it('redirects a non-authenticated user to the singin page if they access elsewhere', () => {
    const authBasedRedirectMiddleware = new AuthBasedRedirectMiddleware();
    const req = {
      path: '/PATH',
      session: {isAuthorized: false}
    };
    return promisifyHandler(authBasedRedirectMiddleware, req).then(({res, next}) => {
      expect(res.redirect).to.have.been.calledWith('/signin');
      expect(next).to.have.been.not.called;
    });
  });

});

function promisifyHandler(authBasedRedirectMiddleware, req) {
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
      authBasedRedirectMiddleware.handle(req, res, next);
    } catch (e) {
      reject(e);
    }
  });

}
