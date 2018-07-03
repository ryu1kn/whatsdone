import GetDonesRequestProcessor from '../../lib/request-processors/GetDones';
import ServiceLocator from '../../lib/ServiceLocator';
import {expect} from 'chai';
import {throwError} from '../helper/TestUtils';
import ServiceFactory from '../../lib/ServiceFactory';
import {request, session} from '../helper/NormalisedRequestData';
import * as td from 'testdouble';
import GetDonesCommand from '../../lib/commands/GetDones';

describe('Server GetDonesRequestProcessor', () => {
  const getDonesCommand = td.object('execute') as GetDonesCommand;
  td.when(getDonesCommand.execute('NEXT_KEY'))
    .thenResolve('COMMAND_OUTPUT');
  td.when(getDonesCommand.execute('CAUSE_ERROR'))
    .thenReject(new Error('UNEXPECTED_ERROR'));

  ServiceLocator.load({createGetDonesCommand: () => getDonesCommand} as ServiceFactory);

  const processor = new GetDonesRequestProcessor();

  it('invokes get dones command with next page key', () => {
    const req = Object.assign({}, request, {query: {nextKey: 'NEXT_KEY'}});
    return processor.process(req, session).then(result => {
      expect(result).to.eql({
        statusCode: '200',
        headers: {'Content-Type': 'application/json'},
        body: '"COMMAND_OUTPUT"'
      });
    });
  });

  it('propagates error', () => {
    const req = Object.assign({}, request, {query: {nextKey: 'CAUSE_ERROR'}});
    return processor.process(req, session).then(
      throwError,
      e => {
        expect(e).to.have.property('message', 'UNEXPECTED_ERROR');
      }
    );
  });
});

