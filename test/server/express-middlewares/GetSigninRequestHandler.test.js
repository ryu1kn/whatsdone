
const GetSigninRequestHandler = require('../../../src/server/express-middlewares/GetSigninRequestHandler');

describe('Server GetSigninRequestHandler', () => {

  it('shows signin page', () => {
    const middleware = new GetSigninRequestHandler();
    const req = {};
    return promisifyExpressMiddleware(middleware, req).then(result => {
      expect(result.res.render).to.have.been.calledWith('signin', {title: 'Sign In - What\'s done?'});
    });
  });

});

