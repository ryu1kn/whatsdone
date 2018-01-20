
import ActionType from './action-type';
import LoginStatus from './login-status';

const initialState = {
  status: LoginStatus.LOGGED_OUT
};

module.exports = (state = initialState, action) => {
  switch (action.type) {
  case ActionType.LOGIN_SUCCESS:
    return {
      status: LoginStatus.LOGGED_IN
    };
  case ActionType.LOGIN_NEW_PASSWORD_REQUIRED:
    return {
      status: LoginStatus.NEW_PASSWORD_REQUIRED,
      cognitoUser: action.cognitoUser
    };
  case ActionType.LOGIN_FAILED:
    console.error(action.error.stack);
    /* fall through */
  case ActionType.LOGIN_REQUEST:
  default:
    return state;
  }
};
