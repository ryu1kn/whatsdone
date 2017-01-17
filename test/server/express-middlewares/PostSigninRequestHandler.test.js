
const PostSigninRequestHandler = require('../../../src/server/express-middlewares/PostSigninRequestHandler');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server PostSigninRequestHandler', () => {

  it('takes user to the root page and update session info if it user exists', () => {
    const userRepository = {
      findUser: sinon.stub().returns(Promise.resolve({id: 'USER_ID'}))
    };
    ServiceLocator.load({
      createUserRepository: () => userRepository
    });
    const middleware = new PostSigninRequestHandler();

    const req = {
      body: 'REQUEST_BODY',
      session: {}
    };
    return promisifyExpressMiddleware(middleware, req).then(({res}) => {
      expect(res.redirect).to.have.been.calledWith('/');
      expect(req.session).to.eql({
        isAuthorized: true,
        userId: 'USER_ID'
      });
      expect(userRepository.findUser).to.have.been.calledWith('REQUEST_BODY');
    });
  });

  it('still shows signin page', () => {
    ServiceLocator.load({
      createUserRepository: () => ({
        findUser: () => Promise.resolve(null)
      })
    });
    const middleware = new PostSigninRequestHandler();

    const req = {session: {}};
    return promisifyExpressMiddleware(middleware, req).then(({res}) => {
      expect(res.status).to.have.been.calledWith(401);
      expect(res.render).to.have.been.calledWith('signin');
      expect(req.session).to.eql({});
    });
  });

  it('propagates error', () => {
    ServiceLocator.load({
      createUserRepository: () => ({
        findUser: () => Promise.reject(new Error('UNEXPECTED_ERROR'))
      })
    });
    const middleware = new PostSigninRequestHandler();

    const req = {session: {}};
    return promisifyExpressMiddleware(middleware, req).then(({next}) => {
      expect(next.args[0][0]).to.have.property('message', 'UNEXPECTED_ERROR');
    });
  });

});

