
const ServiceLocator = require('../lib/ServiceLocator');
const AuthBasedRedirector = require('../lib/AuthBasedRedirector');

describe('Server AuthBasedRedirector', () => {

  it('does nothing if an authenticated user accesses any path', () => {
    const sessionValidator = {validate: sinon.stub().returns(true)};
    ServiceLocator.load({createSessionValidator: () => sessionValidator});
    const authBasedRedirector = new AuthBasedRedirector();
    const request = {path: '/PATH'};
    const session = 'SESSION';
    const result = authBasedRedirector.redirect(request, session);
    expect(result).to.be.null;
    expect(sessionValidator.validate).to.have.been.calledWith('SESSION');
  });

  it('does nothing if a non-authenticated user accesses signin page', () => {
    const sessionValidator = {validate: () => false};
    ServiceLocator.load({createSessionValidator: () => sessionValidator});
    const authBasedRedirector = new AuthBasedRedirector();
    const request = {path: '/signin'};
    const session = 'SESSION';
    const result = authBasedRedirector.redirect(request, session);
    expect(result).to.be.null;
  });

  it('redirects an authenticated user to the root page if they access to the signin page', () => {
    const sessionValidator = {validate: () => true};
    ServiceLocator.load({createSessionValidator: () => sessionValidator});
    const authBasedRedirector = new AuthBasedRedirector();
    const request = {path: '/signin'};
    const session = 'SESSION';
    const result = authBasedRedirector.redirect(request, session);
    expect(result).to.eql({
      statusCode: '303',
      headers: {Location: '/'}
    });
  });

  it('redirects a non-authenticated user to the singin page if they access elsewhere', () => {
    const sessionValidator = {validate: () => false};
    ServiceLocator.load({createSessionValidator: () => sessionValidator});
    const authBasedRedirector = new AuthBasedRedirector();
    const request = {path: '/PATH'};
    const session = 'SESSION';
    const result = authBasedRedirector.redirect(request, session);
    expect(result).to.eql({
      statusCode: '303',
      headers: {Location: '/signin'}
    });
  });

});