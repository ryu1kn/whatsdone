
var AppDispatcher = require('../dispatcher/AppDispatcher');
var DoneConstant = require('../constants/DoneConstant');
var request = require('request');
var q = require('q');

function postDone(doneItem) {
  var deferred = q.defer();
  request.post(window.location.origin + '/dones.json').form(doneItem)
    .on('error', function (error) {
      deferred.reject(error);
    })
    .on('response', function (response) {
      if (response.statusCode !== 200) {
        deferred.reject('status code is not 200');
      }
    })
    .on('data', function (body) {
      deferred.resolve(JSON.parse(body));
    });
  return deferred.promise;
}

/**
 * @param {string} doneId
 */
function deleteDone(doneId) {
  var deferred = q.defer();
  request.del(`${window.location.origin}/dones.json/${doneId}`)
    .on('error', function (error) {
      deferred.reject(error);
    })
    .on('response', function (response) {
      if (response.statusCode !== 200) {
        deferred.reject('status code is not 200');
      }
    })
    .on('data', function (body) {
      deferred.resolve(JSON.parse(body));
    });
  return deferred.promise;
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
