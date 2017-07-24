
export const GET_DONE_REQUEST = Symbol('get-done-request');
export const GET_DONE_SUCCESS = Symbol('get-done-success');
export const GET_DONE_FAILURE = Symbol('get-done-failure');
export const POST_DONE_REQUEST = Symbol('post-done-request');
export const POST_DONE_SUCCESS = Symbol('post-done-success');
export const POST_DONE_FAILURE = Symbol('post-done-failure');
export const DELETE_DONE_REQUEST = Symbol('delete-done-request');
export const DELETE_DONE_SUCCESS = Symbol('delete-done-success');
export const DELETE_DONE_FAILURE = Symbol('delete-done-failure');

export function requestPostDone(item) {
  return {
    type: POST_DONE_REQUEST,
    item
  };
}

export function succeesPostDone(item) {
  return {
    type: POST_DONE_SUCCESS,
    item
  };
}

export function failedPostDone(e) {
  return {
    type: POST_DONE_FAILURE,
    error: e
  };
}

export function requestDeleteDone(id) {
  return {
    type: DELETE_DONE_REQUEST,
    id
  };
}

export function succeesDeleteDone(id) {
  return {
    type: DELETE_DONE_SUCCESS,
    id
  };
}

export function failedDeleteDone(e) {
  return {
    type: DELETE_DONE_FAILURE,
    error: e
  };
}

export function requestGetDones() {
  return {
    type: GET_DONE_REQUEST
  };
}

export function successGetDones(dones) {
  return {
    type: GET_DONE_SUCCESS,
    dones
  };
}

export function failedGetDones(e) {
  return {
    type: GET_DONE_FAILURE,
    error: e
  };
}
