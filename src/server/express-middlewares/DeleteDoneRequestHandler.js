
const ServiceLocator = require('../ServiceLocator');

class DeleteDoneRequestHandler {

  constructor() {
    this._doneRepository = ServiceLocator.doneRepository;
  }

  handle(req, res, next) {
    this._doneRepository.remove(req.params.id, req.session.userId)
      .then(() => {
        res.end();
      })
      .catch(reason => { next(reason); });
  }

}

module.exports = DeleteDoneRequestHandler;
