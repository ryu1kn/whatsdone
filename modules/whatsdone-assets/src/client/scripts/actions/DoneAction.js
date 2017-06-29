
const AppDispatcher = require('../dispatcher/AppDispatcher');
const Const = require('../Const');
const DoneConstant = require('../constants/DoneConstant');
const url = require('url');

function postDone(doneItem) {
  const uri = url.resolve(Const.API_ORIGIN, '/dones');
  const params = {
    method: 'POST',
    headers: {'Accept-Encoding': 'gzip, deflate'},
    body: new FormData(doneItem)
  };
  return fetch(uri, params).then(response => response.json());
}

/**
 * @param {string} doneId
 */
function deleteDone(doneId) {
  const uri = url.resolve(Const.API_ORIGIN, `/dones/${doneId}`);
  const options = {
    method: 'DELETE',
    headers: {'Accept-Encoding': 'gzip, deflate'}
  };
  return fetch(uri, options).then(response => response.json());
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
