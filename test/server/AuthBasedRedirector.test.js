
const AuthBasedRedirector = require('../../src/server/AuthBasedRedirector');

describe('Server AuthBasedRedirector', () => {

  it('does nothing if an authenticated user accesses any path', () => {
    const authBasedRedirector = new AuthBasedRedirector();
    const request = {
      path: '/PATH',
      session: {isAuthorized: true}
    };
    const result = authBasedRedirector.redirect(request);
    expect(result).to.be.null;
  });

  it('does nothing if a non-authenticated user accesses signin page', () => {
    const authBasedRedirector = new AuthBasedRedirector();
    const request = {
      path: '/signin',
      session: {isAuthorized: false}
    };
    const result = authBasedRedirector.redirect(request);
    expect(result).to.be.null;
  });

  it('redirects an authenticated user to the root page if they access to the signin page', () => {
    const authBasedRedirector = new AuthBasedRedirector();
    const request = {
      path: '/signin',
      session: {isAuthorized: true}
    };
    const result = authBasedRedirector.redirect(request);
    expect(result).to.eql({
      statusCode: '303',
      headers: {Location: '/'}
    });
  });

  it('redirects a non-authenticated user to the singin page if they access elsewhere', () => {
    const authBasedRedirector = new AuthBasedRedirector();
    const request = {
      path: '/PATH',
      session: {isAuthorized: false}
    };
    const result = authBasedRedirector.redirect(request);
    expect(result).to.eql({
      statusCode: '303',
      headers: {Location: '/signin'}
    });
  });

});
