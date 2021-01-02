import DeleteDoneRequestProcessor from '../../lib/request-processors/DeleteDone';
import ServiceLocator from '../../lib/ServiceLocator';
import {throwError} from '../helper/TestUtils';
import ServiceFactory from '../../lib/ServiceFactory';
import {request, session} from '../helper/NormalisedRequestData';
import {deepStrictEqual} from 'assert';
import sinon = require('sinon');

describe('Server DeleteDoneRequestProcessor', () => {
  it('deletes a done item', async () => {
    const doneRepository = {remove: sinon.stub().returns(Promise.resolve())};
    ServiceLocator.load({createDoneRepository: () => doneRepository} as ServiceFactory);
    const processor = new DeleteDoneRequestProcessor();

    const response = await processor.process(request, session);
    deepStrictEqual(response, {statusCode: '200'});
    deepStrictEqual(doneRepository.remove.args[0], ['DONE_ID', 'USER_ID']);
  });

  it('propagates error', () => {
    ServiceLocator.load({
      createDoneRepository: () => ({
        remove: () => Promise.reject(new Error('UNEXPECTED_ERROR'))
      })
    } as ServiceFactory);
    const processor = new DeleteDoneRequestProcessor();

    return processor.process(request, session).then(
      throwError,
      e => {
        deepStrictEqual(e.message, 'UNEXPECTED_ERROR');
      }
    );
  });

});

