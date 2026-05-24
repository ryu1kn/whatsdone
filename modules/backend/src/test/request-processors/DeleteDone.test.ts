import DeleteDoneRequestProcessor from '../../lib/request-processors/DeleteDone';
import ServiceLocator from '../../lib/ServiceLocator';
import ServiceFactory from '../../lib/ServiceFactory';
import {request, session} from '../helper/NormalisedRequestData';
import {deepStrictEqual, rejects} from 'assert';
import * as td from 'testdouble';
import DoneRepository from '../../lib/repositories/Done';

describe('Server DeleteDoneRequestProcessor', () => {
  const doneRepository = td.instance(DoneRepository);
  td.when(doneRepository.remove('DONE_ID', 'USER_ID')).thenResolve();
  td.when(doneRepository.remove('DONE_ID_TRIGGER_ERROR', 'USER_ID'))
    .thenReject(new Error('UNEXPECTED_ERROR'));

  ServiceLocator.load({createDoneRepository: () => doneRepository} as ServiceFactory);

  const processor = new DeleteDoneRequestProcessor();

  it('deletes a done item', async () => {
    const response = await processor.process(request, session);

    deepStrictEqual(response, {statusCode: '200'});
  });

  it('propagates error', async () => {
    const problematicRequest = {...request, params: {id: 'DONE_ID_TRIGGER_ERROR'}};

    await rejects(processor.process(problematicRequest, session), new Error('UNEXPECTED_ERROR'));
  });
});

