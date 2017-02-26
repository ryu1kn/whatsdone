
const SchemaBasedRedirectMiddleware = require('../../../src/server/express-middlewares/SchemaBasedRedirect');

describe('Server SchemaBasedRedirectMiddleware', () => {

  it('does nothing if a user is using secure protocol', () => {
    const middleware = new SchemaBasedRedirectMiddleware();
    const req = {
      header: stubWithArgs(['X-Forwarded-Proto'], 'https'),
      hostname: 'HOSTNAME',
      url: '/URL_STRING'
    };
    return promisifyExpressMiddleware(middleware, req).then(result => {
      expect(result.res.redirect).to.have.been.not.called;
      expect(result.next).to.have.been.called;
    });
  });

  it('redirects a user to the https compatible page if they are using insecure protocol', () => {
    const middleware = new SchemaBasedRedirectMiddleware();
    const req = {
      header: stubWithArgs(['X-Forwarded-Proto'], 'http'),
      hostname: 'HOSTNAME',
      url: '/URL_STRING'
    };
    return promisifyExpressMiddleware(middleware, req).then(result => {
      expect(result.res.redirect).to.have.been.calledWith('https://HOSTNAME/URL_STRING');
      expect(result.next).to.have.been.not.called;
    });
  });

});
