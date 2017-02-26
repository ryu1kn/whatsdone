
'use strict';

class NoMatchingRouteRequestHandler {

  handle(req, res) {
    res.status(404);
    res.render('error', {message: '404: Not Found'});
  }

}

module.exports = NoMatchingRouteRequestHandler;
