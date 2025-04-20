import GetDonesRequestProcessor from '../../lib/request-processors/GetDones';
import ServiceLocator from '../../lib/ServiceLocator';
import ServiceFactory from '../../lib/ServiceFactory';
import {request, session} from '../helper/NormalisedRequestData';
import * as td from 'testdouble';
import GetDonesCommand from '../../lib/commands/GetDones';
import {deepStrictEqual, rejects} from 'assert';

describe('Server GetDonesRequestProcessor', () => {
  const commandOutput = {
    items: [{
      id: 'ID',
      date: 'DATE',
      month: 'MONTH',
      userId: 'USER_ID',
      doneThing: 'DONE',
      topics: ['foo']
    }]
  };
  const getDonesCommand = td.object('execute') as GetDonesCommand;
  td.when(getDonesCommand.execute('NEXT_KEY'))
    .thenResolve(commandOutput);
  td.when(getDonesCommand.execute('CAUSE_ERROR'))
    .thenReject(new Error('UNEXPECTED_ERROR'));

  ServiceLocator.load({createGetDonesCommand: () => getDonesCommand} as ServiceFactory);

  const processor = new GetDonesRequestProcessor();

  it('invokes get dones command with next page key', async () => {
    const req = Object.assign({}, request, {query: {nextKey: 'NEXT_KEY'}});
    const result = await processor.process(req, session);
    deepStrictEqual(result, {
      statusCode: '200',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify(commandOutput)
    });
  });

  it('propagates error', async () => {
    const req = {...request, query: {nextKey: 'CAUSE_ERROR'}};

    await rejects(processor.process(req, session), new Error('UNEXPECTED_ERROR'));
  });
});

