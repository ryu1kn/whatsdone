
const ServiceLocator = require('../ServiceLocator');

class UpdateDoneRequestHandler {

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
    this._doneFormatter = ServiceLocator.doneFormatter;
  }

  handle(req, res, next) {
    this._doneRepository.update(req.params.id, req.session.userId, req.body)
      .then(done => {
        res.setHeader('Content-Type', 'application/json');
        res.setHeader('Cache-Control', 'no-cache');
        res.send(this._doneFormatter.format(done));
      })
      .catch(reason => { next(reason); });
  }

}

module.exports = UpdateDoneRequestHandler;
