
const ServiceLocator = require('../ServiceLocator');

class UpdateDoneRequestHandler {

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  handle(req, res, next) {
    this._doneRepository.update(req.params.id, req.session.userId, req.body)
      .then(done => {
        res.setHeader('Content-Type', 'application/json');
        res.setHeader('Cache-Control', 'no-cache');
        res.send(JSON.stringify(done.getAsPlainObject()));
      })
      .catch(reason => { next(reason); });
  }

}

module.exports = UpdateDoneRequestHandler;
