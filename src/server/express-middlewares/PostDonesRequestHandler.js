
const q = require('q');
const ServiceLocator = require('../ServiceLocator');

class PostDonesRequestHandler {

  constructor() {
    this._userRepository = ServiceLocator.userRepository;
    this._doneRepository = ServiceLocator.doneRepository;
  }

  handle(req, res, next) {
    this._doneRepository.write(Object.assign({}, req.body, {userId: req.session.userId}))
      .then(done => this._setUserName(done))
      .then(done => {
        res.setHeader('Content-Type', 'application/json');
        res.setHeader('Cache-Control', 'no-cache');
        res.send(JSON.stringify(done));
      })
      .catch(reason => { next(reason); });
  }

  _setUserName(done) {
    return q(done.userId ? this._userRepository.getById(done.userId) : {name: null})
        .then(user => Object.assign(done, {username: user.name}));
  }

}

module.exports = PostDonesRequestHandler;
