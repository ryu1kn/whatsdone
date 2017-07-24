
import {LOGIN_REQUEST, LOGIN_FAILED, LOGIN_SUCCESS} from './signin/actions';

module.exports = (state = {}, action) => {
  let newState;
  switch (action.type) {
  case LOGIN_REQUEST:
    newState = Object.assign({}, state, {loginStatus: LOGIN_REQUEST});
    break;
  case LOGIN_SUCCESS:
    newState = Object.assign({}, state, {loginStatus: LOGIN_SUCCESS});
    break;
  case LOGIN_FAILED:
    newState = Object.assign({}, state, {loginStatus: LOGIN_FAILED});
    break;
  default:
    newState = {};
    break;
  }
  console.log('new state:', newState);
  return newState;
};
