
import AppDispatcher from '../dispatcher/AppDispatcher';
import {LoginStatusEvent} from '../Const';
import fetchFromWhatsdone from '../FetchFromWhatsdone';

const LoginStatusAction = {

  login: params => {
    AppDispatcher.dispatch({
      actionType: LoginStatusEvent.LOGIN_INITIATED
    });

    login(params.formData).then(() => {
      AppDispatcher.dispatch({
        actionType: LoginStatusEvent.LOGIN_SUCCESSFUL
      });
      params.historyRef.push('/');
    }).catch(e => {
      console.error(e.stack);  // eslint-disable-line no-console
    });
  }

};

function login(formData) {
  const options = {
    method: 'POST',
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
    },
    body: composeSearchParams(formData)
  };
  return fetchFromWhatsdone('/signin', options);
}

function composeSearchParams(formData) {
  const paramArray = [];
  for (var [key, value] of formData.entries()) {
    paramArray.push(`${encodeURIComponent(key)}=${encodeURIComponent(value)}`);
  }
  return paramArray.join('&');
}

module.exports = LoginStatusAction;
