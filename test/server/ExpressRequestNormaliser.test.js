
const ExpressRequestNormaliser = require('../../src/server/ExpressRequestNormaliser');

describe('Server ExpressRequestNormaliser', () => {

  it('extracts http request information as a normalised form', () => {
    const normaliser = new ExpressRequestNormaliser();
    const req = {};
    expect(normaliser.normalise(req)).to.eql({});
  });

});
