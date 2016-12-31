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

app.all('*', (...args) => ServiceLocator.authBasedRedirectMiddleware.handle(...args));

app.use('/', require('./routes/Index'));
app.use('/signin', require('./routes/Signin'));
app.use('/signout', require('./routes/Signout'));
app.use('/dones.json', require('./routes/Dones'));

// catch 404 and forward to error handler
app.use(function (req, res, next) {
  var err = new Error('Not Found');
  err.status = 404;
  next(err);
});

// error handler
app.use((err, req, res, _next) => {
  console.error(err.stack);

  // TODO: Instead of having a rule for error message format
  //       to destinguish error types, define custom exception classes
  var parsedInfo = err.message.match(/^\[([^\]]+)]:.*/);
  var errorKind = parsedInfo && parsedInfo[1];
  var clientMessage;

  switch (errorKind) {
  case 'AccessDeined':
    res.status(403);
    clientMessage = '403: Forbidden';
    break;

  case 'NotFound':
    res.status(404);
    clientMessage = '404: Not Found';
    break;

  default:
    res.status(err.status || 500);
    clientMessage = '500: Internal Server Error';
  }

  res.render('error', {
    message: clientMessage,
    // Print stacktraces only on development
    error: app.get('env') === 'development' ? err : {}
  });
});

module.exports = app;
