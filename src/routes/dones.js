'use strict';

let _ = require('lodash');
let q = require('q');
let express = require('express');
let router = express.Router();

let Dones = require('../models/Dones');
let Users = require('../models/Users');

function setUserName(done) {
  return q(done.userId ? Users.getById(done.userId) : {name: null})
      .then(user => _.assign(done, {username: user.name}));
}

function setUserNames(dones) {
  return Users.getByIds(_.pluck(dones, 'userId'))
    .then(users => {
      let nameMap = _.indexBy(users, '_id');
      return dones.map(done => {
        if (done.userId) {
          done.username = nameMap[done.userId].name;
        }
        return done;
      });
    });
}

router.route('/')
  .get((req, res, next) => {
    Dones.read()
      .then(dones => setUserNames(dones))
      .then(dones => {
        res.setHeader('Content-Type', 'application/json');
        res.send(dones);
      })
      .catch(reason => { next(reason); })
      .done();
  })
  .post((req, res, next) => {
    Dones.write(_.assign({}, req.body, {userId: req.session.userId}))
      .then(done => setUserName(done))
      .then(done => {
        res.setHeader('Content-Type', 'application/json');
        res.setHeader('Cache-Control', 'no-cache');
        res.send(JSON.stringify(done));
      })
      .catch(reason => { next(reason); })
      .done();
  });

router.route('/:id')
  .delete((req, res, next) => {
    Dones.remove(req.params.id, req.session.userId)
      .then(() => {
        res.end();
      })
      .catch(reason => { next(reason); })
      .done();
  })
  .put((req, res, next) => {
    Dones.update(req.params.id, req.session.userId, req.body)
      .then(done => {
        res.setHeader('Content-Type', 'application/json');
        res.setHeader('Cache-Control', 'no-cache');
        res.send(JSON.stringify(done.getAsPlainObject()));
      })
      .catch(reason => { next(reason); })
      .done();
  });

module.exports = router;
