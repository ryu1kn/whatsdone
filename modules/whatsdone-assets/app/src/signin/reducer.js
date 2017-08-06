
import ActionType from './action-type';

const initialState = null;

module.exports = (state = initialState, action) => {
  switch (action.type) {
  case ActionType.LOGIN_FAILED:
    console.error(action.error.stack);
    /* fall through */
  case ActionType.LOGIN_REQUEST:
  case ActionType.LOGIN_SUCCESS:
    return action.type;
  default:
    return state;
  }
};
