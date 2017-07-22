
const AppDispatcher = require('../dispatcher/AppDispatcher');
const DoneConstant = require('../constants/DoneConstant');
const whatsdoneApiClient = require('../whatsdoneApiClient');

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

    whatsdoneApiClient.postDone(doneItem).then(function (updatedItem) {
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
    whatsdoneApiClient.deleteDone(id).then(function () {
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
