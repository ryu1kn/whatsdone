
class GetRootPageRequestHandler {

  handle(req, res) {
    res.render('index', {title: 'What\'s done?'});
  }

}

module.exports = GetRootPageRequestHandler;
