
const GetDonesRequestProcessor = require('../../lib/request-processors/GetDones');
const ServiceLocator = require('../../lib/ServiceLocator');

describe('Server GetDonesRequestProcessor', () => {

  it('returns the output of get dones command result', () => {
    const getDonesCommand = {execute: () => Promise.resolve('COMMAND_OUTPUT')};
    ServiceLocator.load({
      createGetDonesCommand: () => getDonesCommand
    });
    const processor = new GetDonesRequestProcessor();

    const req = {};
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

    const req = {};
    return processor.process(req).then(
      throwError,
      e => {
        expect(e).to.have.property('message', 'UNEXPECTED_ERROR');
      }
    );
  });

});

