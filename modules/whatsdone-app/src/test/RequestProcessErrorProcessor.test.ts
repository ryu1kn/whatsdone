
import RequestProcessErrorProcessor = require('../lib/RequestProcessErrorProcessor');
import ServiceLocator = require('../lib/ServiceLocator');
import {expect} from './TestUtils';
import sinon = require('sinon');

describe('Server RequestProcessErrorProcessor', () => {

  it('shows NOT FOUND page if error indicates so', () => {
    const logger = {error: sinon.spy()};
    ServiceLocator.load({
      createLogger: () => logger
    });
    const processor = new RequestProcessErrorProcessor();
    const result = processor.process(new Error('[NotFound]: NOT_FOUND'));

    expect(result).to.eql({
      statusCode: '404',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({errors: [{title: '404: Not Found'}]})
    });
    expect(logger.error.args[0][0]).to.have.string('Error: [NotFound]: NOT_FOUND');
  });

  it('shows an error page with the information that access was denied', () => {
    const logger = {error: sinon.spy()};
    ServiceLocator.load({
      createLogger: () => logger
    });
    const processor = new RequestProcessErrorProcessor();
    const result = processor.process(new Error('[AccessDenied]: ACCESS_DENIED'));

    expect(result).to.eql({
      statusCode: '403',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({errors: [{title: '403: Forbidden'}]})
    });
    expect(logger.error.args[0][0]).to.have.string('Error: [AccessDenied]: ACCESS_DENIED');
  });

  it('shows a generic error page if an uncategorised error occurred', () => {
    const logger = {error: sinon.spy()};
    ServiceLocator.load({
      createLogger: () => logger
    });
    const processor = new RequestProcessErrorProcessor();
    const result = processor.process(new Error('UNKNOWN ERROR'));

    expect(result).to.eql({
      statusCode: '500',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({errors: [{title: '500: Internal Server Error'}]})
    });
    expect(logger.error.args[0][0]).to.have.string('Error: UNKNOWN');
  });

});
