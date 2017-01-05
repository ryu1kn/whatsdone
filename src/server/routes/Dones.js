'use strict';

let express = require('express');
let router = express.Router();  // eslint-disable-line new-cap

const ServiceLocator = require('../ServiceLocator');
const doneRepository = ServiceLocator.doneRepository;

router.route('/:id')
  .put((req, res, next) => {
    doneRepository.update(req.params.id, req.session.userId, req.body)
      .then(done => {
        res.setHeader('Content-Type', 'application/json');
        res.setHeader('Cache-Control', 'no-cache');
        res.send(JSON.stringify(done.getAsPlainObject()));
      })
      .catch(reason => { next(reason); });
  });

module.exports = router;
