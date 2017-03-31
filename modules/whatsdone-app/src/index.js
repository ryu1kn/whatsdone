
const ServiceLocator = require('./lib/ServiceLocator');
const ServiceFactory = require('./lib/ServiceFactory');

ServiceLocator.load(new ServiceFactory({env: process.env}));

exports.handler = (event, context, callback) => {
  return callback(null, {
    statusCode: '200',
    body: '<html><head><title>TITLE</title></head><body>Hello World</body></html>',
    headers: {
      'Content-Type': 'text/html'
    }
  });
};

exports.getRootPageRequestHandler = (event, context, callback) => {
  const requestHandler = ServiceLocator.getRootPageRequestHandler;
  requestHandler.handle(event, context, callback);
};

exports.getSigninRequestHandler = (event, context, callback) => {
  const requestHandler = ServiceLocator.getSigninRequestHandler;
  requestHandler.handle(event, context, callback);
};

exports.postSigninRequestHandler = (event, context, callback) => {
  const requestHandler = ServiceLocator.postSigninRequestHandler;
  requestHandler.handle(event, context, callback);
};

exports.getDonesRequestHandler = (event, context, callback) => {
  const requestHandler = ServiceLocator.getDonesRequestHandler;
  requestHandler.handle(event, context, callback);
};
