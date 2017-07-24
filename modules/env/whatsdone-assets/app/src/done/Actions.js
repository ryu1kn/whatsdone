
const AppDispatcher = require('./AppDispatcher');
const DoneConstant = require('./Constant');
const ServiceLocator = require('../ServiceLocator');

var DoneAction = {

  create: function (text) {
    var doneItem = {
      doneThing: text,
      date: new Date().toISOString()
    };

    AppDispatcher.dispatch({
      actionType: DoneConstant.DONE_CREATE,
      item: doneItem
    });

    ServiceLocator.whatsdoneApiClient.postDone(doneItem)
      .then(function (updatedItem) {
        AppDispatcher.dispatch({
          actionType: DoneConstant.DONE_CREATE_COMPLETE,
          item: updatedItem
        });
      }).catch(e => {
        console.error(e.stack);  // eslint-disable-line no-console
      });
  },

  destroy: function (id) {
    ServiceLocator.whatsdoneApiClient.deleteDone(id)
      .then(function () {
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
