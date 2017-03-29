
const NoMatchingRouteRequestProcessor = require('../../../src/server/request-processors/NoMatchingRoute');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server NoMatchingRouteRequestProcessor', () => {

  it('returns NOT FOUND reponses', () => {
    const htmlPageGenerator = {generate: sinon.stub().returns('HTML')};
    ServiceLocator.load({createHtmlPageGenerator: () => htmlPageGenerator});
    const processor = new NoMatchingRouteRequestProcessor();

    const request = {};
    const response = processor.process(request);

    expect(response).to.eql({
      statusCode: '404',
      headers: {'Content-Type': 'text/html'},
      body: 'HTML'
    });
    expect(htmlPageGenerator.generate).to.have.been.calledWith('error', {message: '404: Not Found'});
  });

});
