var express = require('express');
// var favicon = require('serve-favicon');
const ServiceFactory = require('./ServiceFactory');
const ServiceLocator = require('./ServiceLocator');

ServiceLocator.load(new ServiceFactory({env: process.env}));

module.exports = function () {
  var app = express();

  // view engine setup
  app.set('views', ServiceLocator.viewDirectoryPath);
  app.set('view engine', 'pug');

  // TODO: uncomment after placing a favicon in /public
  // app.use(favicon(__dirname + '/public/favicon.ico'));
  app.use(ServiceLocator.accessLogger);
  app.use(ServiceLocator.jsonRequestBodyParser);
  app.use(ServiceLocator.encodedUrlParser);
  app.use(ServiceLocator.cookieParser);
  app.use(ServiceLocator.staticContentsProvider);
  app.use(ServiceLocator.sessionManager);

  app.all('*', (...args) => ServiceLocator.schemaBasedRedirectMiddleware.handle(...args));
  app.all('*', (...args) => ServiceLocator.authBasedRedirectMiddleware.handle(...args));
  app.get('/', (...args) => ServiceLocator.getRootPageRequestHandler.handle(...args));
  app.get('/signin', (...args) => ServiceLocator.getSigninRequestHandler.handle(...args));
  app.post('/signin', (...args) => ServiceLocator.postSigninRequestHandler.handle(...args));
  app.get('/signout', (...args) => ServiceLocator.signoutRequestHandler.handle(...args));
  app.post('/signout', (...args) => ServiceLocator.signoutRequestHandler.handle(...args));
  app.get('/dones.json', (...args) => ServiceLocator.getDonesRequestHandler.handle(...args));
  app.post('/dones.json', (...args) => ServiceLocator.postDonesRequestHandler.handle(...args));
  app.delete('/dones.json/:id', (...args) => ServiceLocator.deleteDoneRequestHandler.handle(...args));
  app.put('/dones.json/:id', (...args) => ServiceLocator.updateDoneRequestHandler.handle(...args));
  app.all('*', (...args) => ServiceLocator.noMatchingRouteRequestHandler.handle(...args));

  // To tell express that this is an error handling middleware, we have to give 4 arguments
  app.use((...args) => ServiceLocator.errorHandler.handle(...args));

  return app;
};
