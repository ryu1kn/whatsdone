
import {LOGIN_REQUEST, LOGIN_FAILED, LOGIN_SUCCESS} from './signin/actions';
import {
  GET_DONE_REQUEST, GET_DONE_SUCCESS, GET_DONE_FAILURE,
  POST_DONE_REQUEST, POST_DONE_SUCCESS, POST_DONE_FAILURE,
  DELETE_DONE_REQUEST, DELETE_DONE_FAILURE
} from './done/Actions';

const initialState = {
  dones: [],
  loginStatus: null
};

module.exports = (state = initialState, action) => {
  const newState = reduce(state, action);
  console.log('new state:', newState);
  return newState;
};

function reduce(state, action) {
  switch (action.type) {
  case LOGIN_REQUEST:
    return Object.assign({}, state, {loginStatus: LOGIN_REQUEST});
  case LOGIN_SUCCESS:
    return Object.assign({}, state, {loginStatus: LOGIN_SUCCESS});
  case LOGIN_FAILED:
    return Object.assign({}, state, {loginStatus: LOGIN_FAILED});
  case GET_DONE_REQUEST:
    return state;
  case GET_DONE_SUCCESS:
    return Object.assign({}, state, {
      dones: action.dones.map(normaliseDoneItem).sort(
        (a, b) =>
          a.date < b.date ? 1 :
            a.date > b.date ? -1 : 0
      )
    });
  case POST_DONE_REQUEST:
    return Object.assign({}, state, {
      dones: [...state.dones, normaliseDoneItem(action.item)]
    });
  case POST_DONE_SUCCESS:
    return Object.assign({}, state, {
      dones: updateDones(state.dones, normaliseDoneItem(action.item))
    });
  case DELETE_DONE_REQUEST:
    return Object.assign({}, state, {
      dones: state.dones.filter(done => done.id !== action.id)
    });
  case GET_DONE_FAILURE:
  case POST_DONE_FAILURE:
  case DELETE_DONE_FAILURE:
    console.error(action.error.stack);
    /* fall through */
  default:
    return state;
  }
}

function normaliseDoneItem(doneItem) {
  return Object.assign({}, doneItem, {date: new Date(doneItem.date)});
}

function updateDones(dones, doneItem) {
  const index = dones.findIndex(done => done.date.getTime() === doneItem.date.getTime());
  if (index === -1) return dones;
  return [...dones.slice(0, index), doneItem, ...dones.slice(index + 1)];
}
