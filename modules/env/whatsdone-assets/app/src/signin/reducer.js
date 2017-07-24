
import {LOGIN_REQUEST, LOGIN_FAILED, LOGIN_SUCCESS} from './actions';

const initialState = null;

module.exports = (state = initialState, action) => {
  switch (action.type) {
  case LOGIN_FAILED:
    console.error(action.error.stack);
    /* fall through */
  case LOGIN_REQUEST:
  case LOGIN_SUCCESS:
    return action.type;
  default:
    return state;
  }
};
