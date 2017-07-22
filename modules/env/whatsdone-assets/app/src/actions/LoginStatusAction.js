
import fetchFromWhatsdone from '../FetchFromWhatsdone';

const LoginStatusAction = {login};

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
