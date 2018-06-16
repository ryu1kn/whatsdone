import DeleteDoneRequestProcessor from '../../lib/request-processors/DeleteDone';
import ServiceLocator from '../../lib/ServiceLocator';
import {expect, throwError} from '../helper/TestUtils';
import sinon = require('sinon');
import ServiceFactory from '../../lib/ServiceFactory';
import {request, session} from '../helper/NormalisedRequestData';

describe('Server DeleteDoneRequestProcessor', () => {
  it('deletes a done item', () => {
    const doneRepository = {remove: sinon.stub().returns(Promise.resolve())};
    ServiceLocator.load({createDoneRepository: () => doneRepository} as ServiceFactory);
    const processor = new DeleteDoneRequestProcessor();

    return processor.process(request, session).then(response => {
      expect(response).to.eql({statusCode: '200'});
      expect(doneRepository.remove).to.have.been.calledWith('DONE_ID', 'USER_ID');
    });
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
        expect(e).to.have.property('message', 'UNEXPECTED_ERROR');
      }
    );
  });

});

