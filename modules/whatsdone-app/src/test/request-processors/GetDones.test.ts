
import GetDonesRequestProcessor = require('../../lib/request-processors/GetDones');
import ServiceLocator from '../../lib/ServiceLocator';
import {expect, throwError} from '../TestUtils';
import sinon = require('sinon');

describe('Server GetDonesRequestProcessor', () => {

  it('invokes get dones command with next page key', () => {
    const getDonesCommand = {execute: sinon.stub().returns(Promise.resolve())};
    ServiceLocator.load({
      createGetDonesCommand: () => getDonesCommand
    });
    const processor = new GetDonesRequestProcessor();

    const req = {
      query: {nextKey: 'NEXT_KEY'}
    };
    return processor.process(req).then(() => {
      expect(getDonesCommand.execute).to.have.been.calledWith('NEXT_KEY');
    });
  });

  it('returns the output of get dones command result', () => {
    const getDonesCommand = {execute: () => Promise.resolve('COMMAND_OUTPUT')};
    ServiceLocator.load({
      createGetDonesCommand: () => getDonesCommand
    });
    const processor = new GetDonesRequestProcessor();

    const req = {query: {}};
    return processor.process(req).then(result => {
      expect(result).to.eql({
        statusCode: '200',
        headers: {'Content-Type': 'application/json'},
        body: '"COMMAND_OUTPUT"'
      });
    });
  });

  it('propagates error', () => {
    const getDonesCommand = {execute: () => Promise.reject(new Error('UNEXPECTED_ERROR'))};
    ServiceLocator.load({
      createGetDonesCommand: () => getDonesCommand
    });
    const processor = new GetDonesRequestProcessor();

    const req = {query: {}};
    return processor.process(req).then(
      throwError,
      e => {
        expect(e).to.have.property('message', 'UNEXPECTED_ERROR');
      }
    );
  });

});

