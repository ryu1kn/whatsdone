
const RequestProcessErrorProcessor = require('../../src/server/RequestProcessErrorProcessor');
const ServiceLocator = require('../../src/server/ServiceLocator');

describe('Server RequestProcessErrorProcessor', () => {

  it('shows NOT FOUND page if error indicates so', () => {
    const logger = {error: sinon.spy()};
    const htmlPageGenerator = {generate: sinon.stub().returns('RESPONSE_HTML')};
    ServiceLocator.load({
      createLogger: () => logger,
      createHtmlPageGenerator: () => htmlPageGenerator
    });
    const processor = new RequestProcessErrorProcessor();
    const result = processor.process(new Error('[NotFound]: NOT_FOUND'));

    expect(result).to.eql({
      statusCode: '404',
      headers: {'Content-Type': 'text/html'},
      body: 'RESPONSE_HTML'
    });
    expect(htmlPageGenerator.generate).to.have.been.calledWith('error', {message: '404: Not Found'});
    expect(logger.error.args[0][0]).to.have.string('Error: [NotFound]: NOT_FOUND');
  });

  it('shows an error page with the information that access was denied', () => {
    const logger = {error: sinon.spy()};
    const htmlPageGenerator = {generate: sinon.stub().returns('RESPONSE_HTML')};
    ServiceLocator.load({
      createLogger: () => logger,
      createHtmlPageGenerator: () => htmlPageGenerator
    });
    const processor = new RequestProcessErrorProcessor();
    const result = processor.process(new Error('[AccessDenied]: ACCESS_DENIED'));

    expect(result).to.eql({
      statusCode: '403',
      headers: {'Content-Type': 'text/html'},
      body: 'RESPONSE_HTML'
    });
    expect(htmlPageGenerator.generate).to.have.been.calledWith('error', {message: '403: Forbidden'});
    expect(logger.error.args[0][0]).to.have.string('Error: [AccessDenied]: ACCESS_DENIED');
  });

  it('shows a generic error page if an uncategorised error occurred', () => {
    const logger = {error: sinon.spy()};
    const htmlPageGenerator = {generate: sinon.stub().returns('RESPONSE_HTML')};
    ServiceLocator.load({
      createLogger: () => logger,
      createHtmlPageGenerator: () => htmlPageGenerator
    });
    const processor = new RequestProcessErrorProcessor();
    const result = processor.process(new Error('UNKNOWN ERROR'));

    expect(result).to.eql({
      statusCode: '500',
      headers: {'Content-Type': 'text/html'},
      body: 'RESPONSE_HTML'
    });
    expect(htmlPageGenerator.generate).to.have.been.calledWith('error', {message: '500: Internal Server Error'});
    expect(logger.error.args[0][0]).to.have.string('Error: UNKNOWN');
  });

});
