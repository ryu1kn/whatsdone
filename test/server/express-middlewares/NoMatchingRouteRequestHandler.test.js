
const NoMatchingRouteRequestHandler = require('../../../src/server/express-middlewares/NoMatchingRouteRequestHandler');

describe('NoMatchingRouteRequestHandler', () => {

  it('returns NOT FOUND reponses', () => {
    const middleware = new NoMatchingRouteRequestHandler();
    const req = {};
    return promisifyExpressMiddleware(middleware, req).then(result => {
      expect(result.res.status).to.have.been.calledWith(404);
      expect(result.res.render).to.have.been.calledWith('error', {message: '404: Not Found'});
    });

  });

});
