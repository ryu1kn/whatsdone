
const AppDispatcher = require('../dispatcher/AppDispatcher');
const DoneConstant = require('../constants/DoneConstant');
const fetchFromWhatsdone = require('../FetchFromWhatsdone');

function postDone(doneItem) {
  const options = {
    method: 'POST',
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
    },
    body: composeFormData(doneItem)
  };
  return fetchFromWhatsdone('/dones', options);
}

function composeFormData(data) {
  return Object.keys(data)
    .map(key => `${encodeURIComponent(key)}=${encodeURIComponent(data[key])}`)
    .join('&');
}

/**
 * @param {string} doneId
 */
function deleteDone(doneId) {
  const options = {method: 'DELETE'};
  return fetchFromWhatsdone(`/dones/${doneId}`, options);
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
    }).catch(e => {
      console.error(e.stack);  // eslint-disable-line no-console
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
