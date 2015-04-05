var express = require('express');
var session = require('express-session');
var MongoStore = require('connect-mongo')(session);
var path = require('path');
// var favicon = require('serve-favicon');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');

var config = require('./src/config');

var app = express();

// view engine setup
app.set('views', path.join(__dirname, 'src', 'views'));
app.set('view engine', 'jade');

config.set('dbConnectUrl',
    (process.env.DB_URI_KEY && process.env[process.env.DB_URI_KEY]) ||
    'mongodb://localhost:27017/whatsdone');

// TODO: uncomment after placing a favicon in /public
//app.use(favicon(__dirname + '/public/favicon.ico'));
app.use(logger('dev'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));
app.use(session({
  secret: 'keyboard cat',
  saveUninitialized: false,   // don't create session until something stored
  resave: false,              //don't save session if unmodified
  store: new MongoStore({
    url: config.get('dbConnectUrl'),
    touchAfter: 24 * 3600     // time period in seconds
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

app.use('/', require('./src/routes/index'));
app.use('/signin', require('./src/routes/signin'));
app.use('/signout', require('./src/routes/signout'));
app.use('/dones.json', require('./src/routes/dones'));

// catch 404 and forward to error handler
app.use(function(req, res, next) {
  var err = new Error('Not Found');
  err.status = 404;
  next(err);
});

// error handlers

// development error handler
// will print stacktrace
if (app.get('env') === 'development') {
  app.use(function(err, req, res, next) {
    res.status(err.status || 500);
    res.render('error', {
      message: err.message,
      error: err
    });
  });
}

// production error handler
// no stacktraces leaked to user
app.use(function(err, req, res, next) {
  res.status(err.status || 500);
  res.render('error', {
    message: err.message,
    error: {}
  });
});


module.exports = app;
