
import {
  GET_DONE_REQUEST, GET_DONE_SUCCESS, GET_DONE_FAILURE,
  POST_DONE_REQUEST, POST_DONE_SUCCESS, POST_DONE_FAILURE,
  DELETE_DONE_REQUEST, DELETE_DONE_FAILURE
} from './actions';

const initialState = [];

module.exports = (state = initialState, action) => {
  switch (action.type) {
  case GET_DONE_REQUEST:
    return state;
  case GET_DONE_SUCCESS:
    return action.dones.map(normaliseDoneItem).sort(
      (a, b) =>
        a.date < b.date ? 1 :
        a.date > b.date ? -1 : 0
    );
  case POST_DONE_REQUEST:
    return [...state.dones, normaliseDoneItem(action.item)];
  case POST_DONE_SUCCESS:
    return updateDones(state.dones, normaliseDoneItem(action.item));
  case DELETE_DONE_REQUEST:
    return state.dones.filter(done => done.id !== action.id);
  case GET_DONE_FAILURE:
  case POST_DONE_FAILURE:
  case DELETE_DONE_FAILURE:
    console.error(action.error.stack);
    /* fall through */
  default:
    return state;
  }
};

function normaliseDoneItem(doneItem) {
  return Object.assign({}, doneItem, {date: new Date(doneItem.date)});
}

function updateDones(dones, doneItem) {
  const index = dones.findIndex(done => done.date.getTime() === doneItem.date.getTime());
  if (index === -1) return dones;
  return [...dones.slice(0, index), doneItem, ...dones.slice(index + 1)];
}
