
import ActionType from './action-type';

const initialState = {
  status: null
};

module.exports = (state = initialState, action) => {
  switch (action.type) {
  case ActionType.LOGIN_FAILED:
    console.error(action.error.stack);
    /* fall through */
  case ActionType.LOGIN_REQUEST:
  case ActionType.LOGIN_SUCCESS:
    return {
      status: action.type
    };
  case ActionType.LOGIN_NEW_PASSWORD_REQUIRED:
    return {
      status: 'NEW_PASSWORD_REQUIRED',
      cognitoUser: action.cognitoUser
    };
  default:
    return state;
  }
};
