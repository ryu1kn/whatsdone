
const ExpressResponseSender = require('../../src/server/ExpressResponseSender');

describe('Server ExpressResponseSender', () => {

  it('sends response status code', () => {
    const res = createFakeExpressRes();
    const sender = new ExpressResponseSender({expressRes: res});

    const response = {statusCode: '200'};
    sender.send(response);
    expect(res.status).to.have.been.calledWith('200');
  });

  it('sends response headers', () => {
    const res = createFakeExpressRes();
    const sender = new ExpressResponseSender({expressRes: res});

    const response = {
      headers: {
        'Content-Type': 'text/html',
        OTHER_HEADERS: '..'
      }
    };
    sender.send(response);
    expect(res.set).to.have.been.calledWith({
      'Content-Type': 'text/html',
      OTHER_HEADERS: '..'
    });
  });

  it('sends response body', () => {
    const res = createFakeExpressRes();
    const sender = new ExpressResponseSender({expressRes: res});

    const response = {body: 'RESPONSE_BODY'};
    sender.send(response);
    expect(res.send).to.have.been.calledWith('RESPONSE_BODY');
  });

  function createFakeExpressRes() {
    return {
      status: sinon.spy(),
      set: sinon.spy(),
      send: sinon.spy()
    };
  }

});
