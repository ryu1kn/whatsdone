var express = require('express');
var session = require('express-session');
var DynamoDBStore = require('connect-dynamodb')({session});
var path = require('path');
// var favicon = require('serve-favicon');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');
var context = require('./service/context');

var app = express();
context.setEnv(process.env);

// view engine setup
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'jade');

// TODO: uncomment after placing a favicon in /public
//app.use(favicon(__dirname + '/public/favicon.ico'));
app.use(logger('dev'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, '..', '..', 'public')));
app.use(session({
  secret: 'keyboard cat',
  saveUninitialized: false,   // don't create session until something stored
  resave: false,              // don't save session if unmodified
  store: new DynamoDBStore({
    table: 'whatsdone-sessions',
    client: context.getDynamoDB()
  })
}));

app.all('*', function (req, res, next) {
  if (req.path === '/signin') {
    if (req.session.isAuthorized) {
      res.redirect('/');
    } else {
      next();
    }
  } else {
    if (req.session.isAuthorized) {
      next();
    } else {
      res.redirect('/signin');
    }
  }
});

app.use('/', require('./routes/index'));
app.use('/signin', require('./routes/signin'));
app.use('/signout', require('./routes/signout'));
app.use('/dones.json', require('./routes/dones'));

// catch 404 and forward to error handler
app.use(function(req, res, next) {
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
