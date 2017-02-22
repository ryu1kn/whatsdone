
'use strict';

exports.handler = (event, context, callback) => {
  return callback(null, {
    statusCode: '200',
    body: '<html><head><title>TITLE</title></head><body>Hello World</body></html>',
    headers: {
      'Content-Type': 'text/html'
    }
  });
};
