
const ServiceLocator = require('../../src/server/ServiceLocator');
const ExpressRequestNormaliser = require('../../src/server/ExpressRequestNormaliser');

describe('Server ExpressRequestNormaliser', () => {

  it('extracts http request information as a normalised form', () => {
    const cookieCodec = {extractSessionId: sinon.stub().returns('PARSED_COOKIE')};
    ServiceLocator.load({createCookieCodec: () => cookieCodec});
    const normaliser = new ExpressRequestNormaliser();
    const req = {
      path: 'PATH',
      params: 'QUERY_PARAMS',
      body: 'REQUEST_BODY',
      cookies: 'COOKIE'
    };
    expect(normaliser.normalise(req)).to.eql({
      path: 'PATH',
      params: 'QUERY_PARAMS',
      body: 'REQUEST_BODY',
      sessionId: 'PARSED_COOKIE'
    });
  });

});
