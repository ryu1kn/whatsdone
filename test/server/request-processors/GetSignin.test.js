
const GetSigninRequestProcessor = require('../../../src/server/request-processors/GetSignin');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server GetSigninRequestProcessor', () => {

  it('shows signin page', () => {
    const htmlPageGenerator = {generate: sinon.stub().returns('HTML')};
    ServiceLocator.load({createHtmlPageGenerator: () => htmlPageGenerator});
    const requestProcessor = new GetSigninRequestProcessor();

    const req = {};
    const response = requestProcessor.process(req);
    expect(response).to.eql({
      statusCode: '200',
      body: 'HTML',
      headers: {
        'Content-Type': 'text/html'
      }
    });
    expect(htmlPageGenerator.generate).to.have.been.calledWith('signin', {title: 'Sign In - What\'s done?'});
  });

});

