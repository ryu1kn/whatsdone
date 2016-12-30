'use strict';

let _ = require('lodash');
let q = require('q');
let express = require('express');
let router = express.Router();  // eslint-disable-line new-cap

const ServiceLocator = require('../ServiceLocator');
const doneRepository = ServiceLocator.doneRepository;
const userRepository = ServiceLocator.userRepository;

function setUserName(done) {
  return q(done.userId ? userRepository.getById(done.userId) : {name: null})
      .then(user => Object.assign(done, {username: user.name}));
}

function setUserNames(dones) {
  return userRepository.getByIds(_.map(dones, 'userId'))
    .then(users => {
      let nameMap = _.keyBy(users, 'id');
      return dones.map(done => {
        if (done.userId) {
          done.username = _.get(nameMap, `${done.userId}.name`);
        }
        return done;
      });
    });
}

router.route('/')
  .get((req, res, next) => {
    doneRepository.read()
      .then(dones => setUserNames(dones))
      .then(dones => {
        res.setHeader('Content-Type', 'application/json');
        res.send(dones);
      })
      .catch(reason => { next(reason); });
  })
  .post((req, res, next) => {
    doneRepository.write(Object.assign({}, req.body, {userId: req.session.userId}))
      .then(done => setUserName(done))
      .then(done => {
        res.setHeader('Content-Type', 'application/json');
        res.setHeader('Cache-Control', 'no-cache');
        res.send(JSON.stringify(done));
      })
      .catch(reason => { next(reason); });
  });

router.route('/:id')
  .delete((req, res, next) => {
    doneRepository.remove(req.params.id, req.session.userId)
      .then(() => {
        res.end();
      })
      .catch(reason => { next(reason); });
  })
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
