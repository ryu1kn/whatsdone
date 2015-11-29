'use strict';

let _ = require('lodash');

class Done {

  /**
   * @param {Object} done
   * @return {Object}
   */
  static getModifiable(done) {
    return done => _.pick(done, Done._MODIFIABLE_FIELDS);
  }

  /**
   * @param {string} id
   * @param {string} userId
   * @param {string} doneThing
   * @param {string} date
   */
  constructor(id, userId, doneThing, date) {
    this._id = id;
    this._userId = userId;
    this._doneThing = doneThing;
    this._date = date;
  }
  
  /**
  * @return {{id: string, doneThing: string, date: string, userId: string}}
  */
  getAsPlainObject() {
    return {
      doneThing: this._doneThing,
      date: this._date,
      id: this._id,
      userId: this._userId
    };
  }
}

/**
 * @type {Array<string>}
 */
Done._MODIFIABLE_FIELDS = ['date', 'doneThing'];

module.exports = Done;
