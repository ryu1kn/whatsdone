import PostDoneRequestProcessor from '../../lib/request-processors/PostDone';
import ServiceLocator from '../../lib/ServiceLocator';
import {expect} from '../helper/TestUtils';
import sinon = require('sinon');
import ServiceFactory from '../../lib/ServiceFactory';
import {request} from '../helper/NormalisedRequestData';

describe('Server PostDoneRequestProcessor', () => {

  it('saves a done item', () => {
    const createDoneCommand = {execute: sinon.stub().returns(Promise.resolve('CREATED_DONE'))};
    ServiceLocator.load({createCreateDoneCommand: () => createDoneCommand} as ServiceFactory);
    const processor = new PostDoneRequestProcessor();

    const req = Object.assign({}, request, {
      body: {SOME_DATA: '..'}
    });
    const session = {userId: 'USER_ID', username: 'USER_NAME'};
    return processor.process(req, session).then(response => {
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
