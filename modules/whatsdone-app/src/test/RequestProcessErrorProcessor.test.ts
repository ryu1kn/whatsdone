import RequestProcessErrorProcessor from '../lib/RequestProcessErrorProcessor';
import {Logger} from '../lib/Logger';
import ServiceLocator from '../lib/ServiceLocator';
import {expect} from 'chai';
import ServiceFactory from '../lib/ServiceFactory';
import * as td from 'testdouble';

describe('Server RequestProcessErrorProcessor', () => {

  it('shows NOT FOUND page if error indicates so', () => {
    const logger = td.object('error') as Logger;
    const serviceFactory = td.object('createLogger') as ServiceFactory;
    td.when(serviceFactory.createLogger()).thenReturn(logger);
    ServiceLocator.load(serviceFactory);
    const processor = new RequestProcessErrorProcessor();
    const result = processor.process(new Error('[NotFound]: NOT_FOUND'));

    expect(result).to.eql({
      statusCode: '404',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({errors: [{title: '404: Not Found'}]})
    });
    td.verify(logger.error(td.matchers.contains('Error: [NotFound]: NOT_FOUND')), {ignoreExtraArgs: true});
  });

  it('shows an error page with the information that access was denied', () => {
    const logger = td.object('error') as Logger;
    const serviceFactory = td.object('createLogger') as ServiceFactory;
    td.when(serviceFactory.createLogger()).thenReturn(logger);
    ServiceLocator.load(serviceFactory);
    const processor = new RequestProcessErrorProcessor();
    const result = processor.process(new Error('[AccessDenied]: ACCESS_DENIED'));

    expect(result).to.eql({
      statusCode: '403',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({errors: [{title: '403: Forbidden'}]})
    });
    td.verify(logger.error(td.matchers.contains('Error: [AccessDenied]: ACCESS_DENIED')), {ignoreExtraArgs: true});
  });

  it('shows a generic error page if an uncategorised error occurred', () => {
    const logger = td.object('error') as Logger;
    const serviceFactory = td.object('createLogger') as ServiceFactory;
    td.when(serviceFactory.createLogger()).thenReturn(logger);
    ServiceLocator.load(serviceFactory);
    const processor = new RequestProcessErrorProcessor();
    const result = processor.process(new Error('UNKNOWN ERROR'));

    expect(result).to.eql({
      statusCode: '500',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({errors: [{title: '500: Internal Server Error'}]})
    });
    td.verify(logger.error(td.matchers.contains('Error: UNKNOWN')), {ignoreExtraArgs: true});
  });

});
