
const ServiceLocator = require('./lib/ServiceLocator');
const ServiceFactory = require('./lib/ServiceFactory');

ServiceLocator.load(new ServiceFactory({env: process.env}));

exports.getRootPageRequestHandler = ServiceLocator.getRootPageRequestHandler;

exports.getSigninRequestHandler = ServiceLocator.getSigninRequestHandler;

exports.postSigninRequestHandler = ServiceLocator.postSigninRequestHandler;

exports.getDonesRequestHandler = ServiceLocator.getDonesRequestHandler;
