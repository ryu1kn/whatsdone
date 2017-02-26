
const AuthBasedRedirectMiddleware = require('../../../src/server/express-middlewares/AuthBasedRedirect');

describe('Server AuthBasedRedirectMiddlware', () => {

  it('does nothing if an authenticated user accesses any path', () => {
    const authBasedRedirectMiddleware = new AuthBasedRedirectMiddleware();
    const req = {
      path: '/PATH',
      session: {isAuthorized: true}
    };
    return promisifyExpressMiddleware(authBasedRedirectMiddleware, req).then(result => {
      expect(result.res.redirect).to.have.been.not.called;
      expect(result.next).to.have.been.called;
    });
  });

  it('does nothing if a non-authenticated user accesses signin page', () => {
    const authBasedRedirectMiddleware = new AuthBasedRedirectMiddleware();
    const req = {
      path: '/signin',
      session: {isAuthorized: false}
    };
    return promisifyExpressMiddleware(authBasedRedirectMiddleware, req).then(result => {
      expect(result.res.redirect).to.have.been.not.called;
      expect(result.next).to.have.been.called;
    });
  });

  it('redirects an authenticated user to the root page if they access to the signin page', () => {
    const authBasedRedirectMiddleware = new AuthBasedRedirectMiddleware();
    const req = {
      path: '/signin',
      session: {isAuthorized: true}
    };
    return promisifyExpressMiddleware(authBasedRedirectMiddleware, req).then(result => {
      expect(result.res.redirect).to.have.been.calledWith('/');
      expect(result.next).to.have.been.not.called;
    });
  });

  it('redirects a non-authenticated user to the singin page if they access elsewhere', () => {
    const authBasedRedirectMiddleware = new AuthBasedRedirectMiddleware();
    const req = {
      path: '/PATH',
      session: {isAuthorized: false}
    };
    return promisifyExpressMiddleware(authBasedRedirectMiddleware, req).then(result => {
      expect(result.res.redirect).to.have.been.calledWith('/signin');
      expect(result.next).to.have.been.not.called;
    });
  });

});
