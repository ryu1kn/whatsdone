var _ = require('lodash');

/**
 * @constructor
 * @param {string} id
 * @param {string} userId
 * @param {string} doneThing
 * @param {string} date
 */
function Done(id, userId, doneThing, date) {
  this._id = id;
  this._userId = userId;
  this._doneThing = doneThing;
  this._date = date;
}

/**
 * @type {Array<string>}
 */
Done._MODIFIABLE_FIELDS = ['date', 'doneThing'];

/**
 * @param {Object} done
 * @return {Object}
 */
Done.getModifiable = done => _.pick(done, Done._MODIFIABLE_FIELDS);

/**
 * @return {{id: string, doneThing: string, date: string, userId: string}}
 */
Done.prototype.getAsPlainObject = function () {
  return {
    doneThing: this._doneThing,
    date: this._date,
    id: this._id,
    userId: this._userId
  };
};

module.exports = Done;
