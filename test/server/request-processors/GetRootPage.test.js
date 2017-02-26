
const GetRootPageRequestProcessor = require('../../../src/server/request-processors/GetRootPage');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server GetRootPageRequestProcessor', () => {

  it('shows root page', () => {
    const htmlPageGenerator = {generate: sinon.stub().returns('HTML')};
    ServiceLocator.load({createHtmlPageGenerator: () => htmlPageGenerator});
    const requestProcessor = new GetRootPageRequestProcessor();

    const req = {};
    const response = requestProcessor.process(req);
    expect(response).to.eql({
      statusCode: '200',
      body: 'HTML',
      headers: {
        'Content-Type': 'text/html'
      }
    });
    expect(htmlPageGenerator.generate).to.have.been.calledWith('index', {title: "What's done?"});
  });

});
