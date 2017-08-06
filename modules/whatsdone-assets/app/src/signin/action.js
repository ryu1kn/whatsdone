
import ActionType from './action-type';

export default {login, markLoginSuccess, markLoginFailed};

function login(loginDetails) {
  return {
    type: ActionType.LOGIN_REQUEST,
    loginDetails
  };
}

function markLoginSuccess() {
  return {
    type: ActionType.LOGIN_SUCCESS
  };
}

function markLoginFailed(e) {
  return {
    type: ActionType.LOGIN_FAILED,
    error: e
  };
}
