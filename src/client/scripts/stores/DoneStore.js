
var AppDispatcher = require('../dispatcher/AppDispatcher');
var EventEmitter = require('events').EventEmitter;
var DoneConstant = require('../constants/DoneConstant');
var request = require('request-promise');

var CHANGE_EVENT = 'change';

// TODO: Change _dones from an array to an object
var _dones = [];

function load() {
  return request.get(`${window.location.origin}/dones.json`)
    .then(body => JSON.parse(body));
}

// TODO: Instead of defining normalise functions,
//       define DoneItem class and make it deal with own data properly

/**
 * @param {{doneThing: string, date: (string|Date)}} doneItem
 * @return {{doneThing: string, date: Date}}
 */
function normaliseDoneItem(doneItem) {
  return Object.assign({}, doneItem, {date: new Date(doneItem.date)});
}

/**
 * @param {Array<{doneThing: string, date: string, ...}> doneItems
 * @return {Array<{doneThing: string, date: Date, ...}>
 */
function normaliseDoneItems(doneItems) {
  return doneItems.map(normaliseDoneItem);
}

/**
 * @param {{doneThing: string, date: string}} doneItem
 */
function add(doneItem) {
  _dones.push(normaliseDoneItem(doneItem));
}

/**
 * @param {{doneThing: string, date: string}} doneItem
 */
function update(doneItem) {
  doneItem = normaliseDoneItem(doneItem);
  var found = _dones.filter((done) =>
                  done.date.getTime() === doneItem.date.getTime());
  if (found && found.length > 0) {
    Object.assign(found[0], doneItem);
  }
}

/**
 * TODO: Make it able to revert. Consider the case of delete request failure
 * @param {string} id
 */
function destroy(id) {
  _dones = _dones.filter((done) => done.id !== id);
}

var DoneStore = Object.assign({}, EventEmitter.prototype, {

  /**
   * Get the entire collection of DONEs.
   * @return {object}
   */
  getAll: function () {
    return _dones.sort(
              (a, b) =>
                  a.date < b.date ?  1 :
                  a.date > b.date ? -1 : 0);
  },

  load: function () {
    var me = this;
    load().then((response) => {
      _dones = normaliseDoneItems(response);
      me.emit(CHANGE_EVENT);
    }).catch(function (error) {
      console.error(error);
    });
  },

  emitChange: function () {
    this.emit(CHANGE_EVENT);
  },

  /**
   * @param {function} callback
   */
  addChangeListener: function (callback) {
    this.on(CHANGE_EVENT, callback);
  },

  /**
   * @param {function} callback
   */
  removeChangeListener: function (callback) {
    this.removeListener(CHANGE_EVENT, callback);
  }
});

// Register callback to handle all updates
AppDispatcher.register(function (action) {
  switch (action.actionType) {
    case DoneConstant.DONE_CREATE:
      add(action.item);
      DoneStore.emitChange();
      break;

    case DoneConstant.DONE_CREATE_COMPLETE:
      update(action.item);
      DoneStore.emitChange();
      break;

    case DoneConstant.DONE_DESTROY:
      destroy(action.id);
      DoneStore.emitChange();
      break;

    default:
      // no op
  }
});

module.exports = DoneStore;
