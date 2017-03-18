
const SignoutRequestProcessor = require('../../../src/server/request-processors/Signout');

describe('Server SignoutRequestProcessor', () => {

  it('removes authorised flag and shows user the signin page', () => {
    const processor = new SignoutRequestProcessor();
    const request = {
      session: {isAuthorized: true}
    };
    const response = processor.process(request);

    expect(response).to.eql({
      statusCode: '303',
      headers: {
        Location: '/signin'
      }
    });
    expect(request.session).to.eql({});
  });

});
