
import PostDoneRequestProcessor = require('../../lib/request-processors/PostDone');
import ServiceLocator = require('../../lib/ServiceLocator');
import {expect} from '../TestUtils';
import sinon = require('sinon');

describe('Server PostDoneRequestProcessor', () => {

  it('saves a done item', () => {
    const createDoneCommand = {execute: sinon.stub().returns(Promise.resolve('CREATED_DONE'))};
    ServiceLocator.load({createCreateDoneCommand: () => createDoneCommand});
    const processor = new PostDoneRequestProcessor();

    const request = {
      body: {SOME_DATA: '..'}
    };
    const session = {userId: 'USER_ID', username: 'USER_NAME'};
    return processor.process(request, session).then(response => {
      expect(response).to.eql({
        statusCode: '200',
        headers: {'Content-Type': 'application/json'},
        body: '"CREATED_DONE"'
      });
      expect(createDoneCommand.execute).to.have.been.calledWith({
        data: {SOME_DATA: '..'},
        userId: 'USER_ID',
        username: 'USER_NAME'
      });
    });
  });

});
