
const ExpressRequestNormaliser = require('../../src/server/ExpressRequestNormaliser');

describe('Server ExpressRequestNormaliser', () => {

  it('extracts http request information as a normalised form', () => {
    const normaliser = new ExpressRequestNormaliser();
    const req = {
      params: 'QUERY_PARAMS',
      body: 'REQUEST_BODY',
      session: 'SESSION'
    };
    expect(normaliser.normalise(req)).to.eql({
      params: 'QUERY_PARAMS',
      body: 'REQUEST_BODY',
      session: 'SESSION'
    });
  });

});
