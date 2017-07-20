
const fetch = require('node-fetch');

module.exports = (event, context, callback) => {

  console.log('Received event', JSON.stringify(event));

  const shouldFail = event.ResourceProperties.shouldFail && event.RequestType !== 'Delete';

  const body = JSON.stringify({
    Status: shouldFail ? 'FAILED' : 'SUCCESS',
    Reason: shouldFail ? '' : 'Failed',
    PhysicalResourceId: 'testResource',
    StackId: event.StackId,
    RequestId: event.RequestId,
    LogicalResourceId: event.LogicalResourceId,
    Data: shouldFail ? {} : {foo: 'bar'}
  });

  const options = {
    method: 'put',
    body,
    headers: {
      'content-type': '',
      'content-length': body.length
    }
  };
  fetch(event.ResponseURL, options)
    .then(response => {
      if (response.status !== 200) throw new Error(`Invalid response code ${response.status}`);
      return response.text();
    })
    .then(responseText => {
      console.log('Response text:', responseText);
    })
    .catch(callback);
};
