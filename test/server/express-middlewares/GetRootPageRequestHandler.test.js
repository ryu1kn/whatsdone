
const GetRootPageRequestHandler = require('../../../src/server/express-middlewares/GetRootPageRequestHandler');

describe('Server GetRootPageRequestHandler', () => {

  it('shows root page', () => {
    const middleware = new GetRootPageRequestHandler();
    const req = {};
    return promisifyExpressMiddleware(middleware, req).then(result => {
      expect(result.res.render).to.have.been.calledWith('index', {title: 'What\'s done?'});
    });
  });

});

