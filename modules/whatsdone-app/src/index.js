
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

exports.getRootPageRequestHandler = ServiceLocator.getRootPageRequestHandler;

exports.getSigninRequestHandler = ServiceLocator.getSigninRequestHandler;

exports.postSigninRequestHandler = ServiceLocator.postSigninRequestHandler;

exports.getDonesRequestHandler = ServiceLocator.getDonesRequestHandler;
