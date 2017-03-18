
const ExpressRequestNormaliser = require('../../src/server/ExpressRequestNormaliser');

describe('Server ExpressRequestNormaliser', () => {

  it('extracts http request information as a normalised form', () => {
    const normaliser = new ExpressRequestNormaliser();
    const req = {
      path: 'PATH',
      params: 'QUERY_PARAMS',
      body: 'REQUEST_BODY',
      session: 'SESSION'
    };
    expect(normaliser.normalise(req)).to.eql({
      path: 'PATH',
      params: 'QUERY_PARAMS',
      body: 'REQUEST_BODY',
      session: 'SESSION'
    });
  });

});
