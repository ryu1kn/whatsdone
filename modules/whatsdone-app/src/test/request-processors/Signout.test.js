
const SignoutRequestProcessor = require('../../lib/request-processors/Signout');

describe('Server SignoutRequestProcessor', () => {

  it('removes session', () => {
    const processor = new SignoutRequestProcessor();
    const request = {};
    const session = {};

    const response = processor.process(request, session);
    expect(response).to.eql({
      statusCode: '200'
    });
  });

});
