
const ServiceLocator = require('../../src/server/ServiceLocator');
const AuthBasedRedirector = require('../../src/server/AuthBasedRedirector');
const SessionValidator = require('../../src/server/SessionValidator');

describe('Server AuthBasedRedirector', () => {


  it('does nothing if an authenticated user accesses any path', () => {
    ServiceLocator.load({
      createSessionValidator: () => new SessionValidator()
    });
    const authBasedRedirector = new AuthBasedRedirector();
    const request = {path: '/PATH'};
    const session = {isAuthorized: true};
    const result = authBasedRedirector.redirect(request, session);
    expect(result).to.be.null;
  });

  it('does nothing if a non-authenticated user accesses signin page', () => {
    ServiceLocator.load({
      createSessionValidator: () => new SessionValidator()
    });
    const authBasedRedirector = new AuthBasedRedirector();
    const request = {path: '/signin'};
    const session = {isAuthorized: false};
    const result = authBasedRedirector.redirect(request, session);
    expect(result).to.be.null;
  });

  it('redirects an authenticated user to the root page if they access to the signin page', () => {
    ServiceLocator.load({
      createSessionValidator: () => new SessionValidator()
    });
    const authBasedRedirector = new AuthBasedRedirector();
    const request = {path: '/signin'};
    const session = {isAuthorized: true};
    const result = authBasedRedirector.redirect(request, session);
    expect(result).to.eql({
      statusCode: '303',
      headers: {Location: '/'}
    });
  });

  it('redirects a non-authenticated user to the singin page if they access elsewhere', () => {
    ServiceLocator.load({
      createSessionValidator: () => new SessionValidator()
    });
    const authBasedRedirector = new AuthBasedRedirector();
    const request = {path: '/PATH'};
    const session = {isAuthorized: false};
    const result = authBasedRedirector.redirect(request, session);
    expect(result).to.eql({
      statusCode: '303',
      headers: {Location: '/signin'}
    });
  });

  it('treats session not exists case as non-authenticated', () => {
    ServiceLocator.load({
      createSessionValidator: () => new SessionValidator()
    });
    const authBasedRedirector = new AuthBasedRedirector();
    const request = {path: '/PATH'};
    const session = null;
    const result = authBasedRedirector.redirect(request, session);
    expect(result).to.eql({
      statusCode: '303',
      headers: {Location: '/signin'}
    });
  });

});
