var express = require('express');
var session = require('express-session');
var DynamoDBStore = require('connect-dynamodb')({session});
var path = require('path');
// var favicon = require('serve-favicon');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');
const ServiceFactory = require('./ServiceFactory');
const ServiceLocator = require('./ServiceLocator');

module.exports = function () {
  var app = express();

  ServiceLocator.load(new ServiceFactory({env: process.env}));

  // view engine setup
  app.set('views', path.join(__dirname, 'views'));
  app.set('view engine', 'pug');

  // TODO: uncomment after placing a favicon in /public
  // app.use(favicon(__dirname + '/public/favicon.ico'));
  app.use(logger('dev'));
  app.use(bodyParser.json());
  app.use(bodyParser.urlencoded({extended: false}));
  app.use(cookieParser());
  app.use(express.static(path.join(__dirname, '..', '..', 'public')));
  app.use(session({
    secret: process.env.SESSION_SECRET,
    saveUninitialized: false,   // don't create session until something stored
    resave: false,              // don't save session if unmodified
    store: new DynamoDBStore({
      table: 'whatsdone-sessions',
      client: ServiceLocator.dynamoDB
    })
  }));

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
