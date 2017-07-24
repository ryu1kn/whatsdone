
const AppDispatcher = require('./AppDispatcher');
const EventEmitter = require('events').EventEmitter;
const DoneConstant = require('./Constant');
const ServiceLocator = require('../ServiceLocator');

var CHANGE_EVENT = 'change';

// TODO: Change _dones from an array to an object
var _dones = [];

// TODO: Instead of defining normalise functions,
//       define DoneItem class and make it deal with own data properly

function normaliseDoneItem(doneItem) {
  return Object.assign({}, doneItem, {date: new Date(doneItem.date)});
}

function normaliseDoneItems(doneItems) {
  return doneItems.map(normaliseDoneItem);
}

function add(doneItem) {
  _dones.push(normaliseDoneItem(doneItem));
}

function update(doneItem) {
  doneItem = normaliseDoneItem(doneItem);
  var found = _dones.filter(done => done.date.getTime() === doneItem.date.getTime());
  if (found && found.length > 0) {
    Object.assign(found[0], doneItem);
  }
}

// TODO: Make it able to revert. Consider the case of delete request failure
function destroy(id) {
  _dones = _dones.filter(done => done.id !== id);
}

var DoneStore = Object.assign({}, EventEmitter.prototype, {

  getAll: function () {
    return _dones.sort(
              (a, b) =>
                  a.date < b.date ? 1 :
                  a.date > b.date ? -1 : 0);
  },

  load: function () {
    var me = this;
    ServiceLocator.whatsdoneApiClient.getDones().then(response => {
      _dones = normaliseDoneItems(response);
      me.emit(CHANGE_EVENT);
    }).catch(e => {
      console.error(e.stack);
    });
  },

  emitChange: function () {
    this.emit(CHANGE_EVENT);
  },

  addChangeListener: function (callback) {
    this.on(CHANGE_EVENT, callback);
  },

  removeChangeListener: function (callback) {
    this.removeListener(CHANGE_EVENT, callback);
  }
});

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
