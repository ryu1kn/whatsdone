
const NoMatchingRouteRequestHandler = require('../../../src/server/express-middlewares/NoMatchingRouteRequestHandler');

describe('NoMatchingRouteRequestHandler', () => {

  it('returns NOT FOUND reponses', () => {
    const middleware = new NoMatchingRouteRequestHandler();
    const req = {};
    return promisifyExpressMiddleware(middleware, req).then(({res}) => {
      expect(res.status).to.have.been.calledWith(404);
      expect(res.render).to.have.been.calledWith(
        'error',
        {
          message: '404: Not Found',
          error: {}
        }
      );
    });

  });

});
