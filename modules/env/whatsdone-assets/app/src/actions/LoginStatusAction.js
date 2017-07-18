
import AppDispatcher from '../dispatcher/AppDispatcher';
import {LoginStatusEvent} from '../Const';
import fetchFromWhatsdone from '../FetchFromWhatsdone';

const LoginStatusAction = {

  login: params => {
    AppDispatcher.dispatch({
      actionType: LoginStatusEvent.LOGIN_INITIATED
    });

    login(params.loginDetails).then(() => {
      AppDispatcher.dispatch({
        actionType: LoginStatusEvent.LOGIN_SUCCESSFUL
      });
      params.historyRef.push('/');
    }).catch(e => {
      console.error(e.stack);  // eslint-disable-line no-console
    });
  }

};

function login(loginDetails) {
  const options = {
    method: 'POST',
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
    },
    body: composeSearchParams(loginDetails)
  };
  return fetchFromWhatsdone('/signin', options);
}

function composeSearchParams(loginDetails) {
  return Object.keys(loginDetails)
    .map(key => `${encodeURIComponent(key)}=${encodeURIComponent(loginDetails[key])}`)
    .join('&');
}

module.exports = LoginStatusAction;
