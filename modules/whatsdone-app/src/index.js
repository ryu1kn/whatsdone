
exports.handler = (event, context, callback) => {
  console.log('JSON.stringify(event):', JSON.stringify(event));
  console.log('JSON.stringify(context):', JSON.stringify(context));
  return callback(null, {
    statusCode: '200',
    body: '<html><head><title>TITLE</title></head><body>Hello World</body></html>',
    headers: {
      'Content-Type': 'text/html'
    }
  });
};
