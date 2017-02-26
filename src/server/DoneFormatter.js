
'use strict';

const _ = require('lodash');

class DoneFormatter {

  format(done) {
    return JSON.stringify(_.pick(done, ['id', 'userId', 'date', 'doneThing']));
  }

}

module.exports = DoneFormatter;
