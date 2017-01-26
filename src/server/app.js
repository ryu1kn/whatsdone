
const ServiceFactory = require('./ServiceFactory');
const ServiceLocator = require('./ServiceLocator');

ServiceLocator.load(new ServiceFactory({env: process.env}));

module.exports = app => {
  // view engine setup
  app.set('views', ServiceLocator.viewDirectoryPath);
  app.set('view engine', 'pug');

  app.use(ServiceLocator.accessLogger);
  app.use(ServiceLocator.jsonRequestBodyParser);
  app.use(ServiceLocator.encodedUrlParser);
  app.use(ServiceLocator.cookieParser);
  app.use(ServiceLocator.staticContentsProvider);
  app.use(ServiceLocator.faviconProvider);
  app.use(ServiceLocator.sessionManager);

  app.all('*', ServiceLocator.schemaBasedRedirectMiddleware);
  app.all('*', ServiceLocator.authBasedRedirectMiddleware);
  app.get('/', ServiceLocator.getRootPageRequestHandler);
  app.get('/signin', ServiceLocator.getSigninRequestHandler);
  app.post('/signin', ServiceLocator.postSigninRequestHandler);
  app.get('/signout', ServiceLocator.signoutRequestHandler);
  app.post('/signout', ServiceLocator.signoutRequestHandler);
  app.get('/dones.json', ServiceLocator.getDonesRequestHandler);
  app.post('/dones.json', ServiceLocator.postDonesRequestHandler);
  app.delete('/dones.json/:id', ServiceLocator.deleteDoneRequestHandler);
  app.put('/dones.json/:id', ServiceLocator.updateDoneRequestHandler);
  app.all('*', ServiceLocator.noMatchingRouteRequestHandler);

  // To tell express that this is an error handling middleware, we have to give 4 arguments
  app.use(ServiceLocator.errorHandler);
};
