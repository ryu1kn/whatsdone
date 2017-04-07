
const LambdaResponseFormatter = require('../lib/LambdaResponseFormatter');

describe('Server LambdaResponseFormatter', () => {

  it('passes through as standard response format IS lambda\'s response format', () => {
    const formatter = new LambdaResponseFormatter();

    const response = 'STANDARD_RESPONSE';
    expect(formatter.format(response)).to.eql('STANDARD_RESPONSE');
  });

});
