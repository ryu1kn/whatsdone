
const AppDispatcher = require('../dispatcher/AppDispatcher');
const DoneConstant = require('../constants/DoneConstant');
const url = require('url');
const sendAjax = require('../SendAjax');

function postDone(doneItem) {
  const uri = url.resolve(WhatsDone.API_ORIGIN, '/dones');
  const options = {
    method: 'POST',
    body: new FormData(doneItem)
  };
  return sendAjax(uri, options);
}

/**
 * @param {string} doneId
 */
function deleteDone(doneId) {
  const uri = url.resolve(WhatsDone.API_ORIGIN, `/dones/${doneId}`);
  const options = {method: 'DELETE'};
  return sendAjax(uri, options);
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
