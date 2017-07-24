
export const LOGIN_REQUEST = Symbol('login-request');

export function requestLogin(loginDetails) {
  return {
    type: LOGIN_REQUEST,
    loginDetails
  };
}

export const LOGIN_SUCCESS = Symbol('login-success');

export function successLogin() {
  return {
    type: LOGIN_SUCCESS
  };
}

export const LOGIN_FAILED = Symbol('login-failed');

export function failedLogin(e) {
  return {
    type: LOGIN_FAILED,
    error: e
  };
}
