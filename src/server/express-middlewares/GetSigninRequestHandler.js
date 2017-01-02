
class GetSigninRequestHandler {

  handle(req, res) {
    res.render('signin', {title: 'Sign In - What\'s done?'});
  }

}

module.exports = GetSigninRequestHandler;
