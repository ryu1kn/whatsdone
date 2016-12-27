
var AppDispatcher = require('../dispatcher/AppDispatcher');
var DoneConstant = require('../constants/DoneConstant');
var request = require('request-promise');

function postDone(doneItem) {
  const options = {
    method: 'POST',
    uri: `${window.location.origin}/dones.json`,
    form: doneItem
  };
  return request(options).then(body => JSON.parse(body));
}

/**
 * @param {string} doneId
 */
function deleteDone(doneId) {
  const options = {
    method: 'DELETE',
    uri: `${window.location.origin}/dones.json/${doneId}`
  };
  return request(options).then(body => JSON.parse(body));
}

var DoneAction = {

  /**
   * @param {string} text
   */
  create: function (text) {
    var doneItem = {
      doneThing: text,
      date: new Date().toISOString()
    };

    AppDispatcher.dispatch({
      actionType: DoneConstant.DONE_CREATE,
      item: doneItem
    });

    postDone(doneItem).then(function (updatedItem) {
      AppDispatcher.dispatch({
        actionType: DoneConstant.DONE_CREATE_COMPLETE,
        item: updatedItem
      });
    }).catch(function (reason) {
      console.error(reason);  // eslint-disable-line no-console
    });
  },

  /**
   * @param {string} id
   */
  destroy: function (id) {
    deleteDone(id).then(function () {
      AppDispatcher.dispatch({
        actionType: DoneConstant.DONE_DESTROY,
        id: id
      });
    }).catch(function () {
      // TODO: Rollback the deletion
    });
  }

};

module.exports = DoneAction;
