
const SignoutRequestHandler = require('../../../src/server/express-middlewares/SignoutRequestHandler');

describe('Server SignoutRequestHandler', () => {

  it('removes authorised flag and shows user the signin page', () => {
    const middleware = new SignoutRequestHandler();
    const req = {
      session: {isAuthorized: true}
    };
    return promisifyExpressMiddleware(middleware, req).then(result => {
      expect(result.res.redirect).to.have.been.calledWith('/signin');
      expect(req.session).to.eql({});
    });
  });

});

