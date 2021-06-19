import ActionType from './action-type';

export default {
  getDones,
  markGetDonesSuccess,
  markGetDonesFailed,
  postDone,
  markPostDoneSuccess,
  markPostDoneFailed,
  deleteDone,
  markDeleteDoneSuccess,
  markDeleteDoneFailed
};

function postDone(item) {
  return {
    type: ActionType.POST_DONE_REQUEST,
    item
  };
}

function markPostDoneSuccess(item) {
  return {
    type: ActionType.POST_DONE_SUCCESS,
    item
  };
}

function markPostDoneFailed(e) {
  return {
    type: ActionType.POST_DONE_FAILURE,
    error: e
  };
}

function deleteDone(id) {
  return {
    type: ActionType.DELETE_DONE_REQUEST,
    id
  };
}

function markDeleteDoneSuccess(id) {
  return {
    type: ActionType.DELETE_DONE_SUCCESS,
    id
  };
}

function markDeleteDoneFailed(e) {
  return {
    type: ActionType.DELETE_DONE_FAILURE,
    error: e
  };
}

function getDones() {
  return {
    type: ActionType.GET_DONE_REQUEST
  };
}

function markGetDonesSuccess(dones, nextKey) {
  return {
    type: ActionType.GET_DONE_SUCCESS,
    dones,
    nextKey
  };
}

function markGetDonesFailed(e) {
  return {
    type: ActionType.GET_DONE_FAILURE,
    error: e
  };
}
