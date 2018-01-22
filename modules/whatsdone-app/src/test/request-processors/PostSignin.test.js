
const PostSigninRequestProcessor = require('../../lib/request-processors/PostSignin');

describe('Server PostSigninRequestProcessor', () => {

  it('updates session info if the user exists', () => {
    const processor = new PostSigninRequestProcessor();

    const request = {};
    const result = processor.process(request);
    expect(result).to.eql({
      statusCode: '200',
      headers: {
        'Content-Type': 'text/plain'
      }
    });
  });

});

