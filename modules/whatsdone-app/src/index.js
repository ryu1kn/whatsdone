
const ServiceLocator = require('./lib/ServiceLocator');
const ServiceFactory = require('./lib/ServiceFactory');

ServiceLocator.load(new ServiceFactory({env: process.env}));

exports.getRootPageRequestHandler = ServiceLocator.getRootPageRequestHandler;

exports.getSigninRequestHandler = ServiceLocator.getSigninRequestHandler;

exports.postSigninRequestHandler = ServiceLocator.postSigninRequestHandler;

exports.signoutRequestHandler = ServiceLocator.signoutRequestHandler;

exports.getDonesRequestHandler = ServiceLocator.getDonesRequestHandler;

exports.postDoneRequestHandler = ServiceLocator.postDoneRequestHandler;

exports.deleteDoneRequestHandler = ServiceLocator.deleteDoneRequestHandler;

exports.updateDoneRequestHandler = ServiceLocator.updateDoneRequestHandler;
