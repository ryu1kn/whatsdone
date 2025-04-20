import ActionType from './action-type';
import {PostDoneItem} from '../whatsdone-api-client';
import {NextKey, RawDoneItem} from './reducer';

export default {
  getDones,
  markGetDonesSuccess,
  markGetDonesFailed,
  postDone,
  markPostDoneSuccess,
  markPostDoneFailed,
  deleteDone,
  markDeleteDoneSuccess,
  markDeleteDoneFailed,
  startEditDone,
  updateDone,
  markUpdateDoneSuccess,
  markUpdateDoneFailed
};

function postDone(item: PostDoneItem) {
  return {
    type: ActionType.POST_DONE_REQUEST,
    item
  };
}

function markPostDoneSuccess(item: RawDoneItem) {
  return {
    type: ActionType.POST_DONE_SUCCESS,
    item
  };
}

function markPostDoneFailed(e: Error) {
  return {
    type: ActionType.POST_DONE_FAILURE,
    error: e
  };
}

function deleteDone(id: string) {
  return {
    type: ActionType.DELETE_DONE_REQUEST,
    id
  };
}

function markDeleteDoneSuccess(id: string) {
  return {
    type: ActionType.DELETE_DONE_SUCCESS,
    id
  };
}

function markDeleteDoneFailed(e: Error) {
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

function markGetDonesSuccess(dones: RawDoneItem[], nextKey: NextKey) {
  return {
    type: ActionType.GET_DONE_SUCCESS,
    dones,
    nextKey
  };
}

function markGetDonesFailed(e: Error) {
  return {
    type: ActionType.GET_DONE_FAILURE,
    error: e
  };
}

function startEditDone(id: string) {
  return {
    type: ActionType.START_EDIT_DONE,
    id
  };
}

function updateDone(id: string, doneThing: string) {
  return {
    type: ActionType.UPDATE_DONE_REQUEST,
    id,
    doneThing
  };
}

function markUpdateDoneSuccess(id: string) {
  return {
    type: ActionType.UPDATE_DONE_SUCCESS,
    id
  };
}

function markUpdateDoneFailed(e: Error) {
  return {
    type: ActionType.UPDATE_DONE_FAILURE,
    error: e
  };
}
