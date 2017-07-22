import {connect} from 'react-redux';
import SigninPage from './page';
import fetchFromWhatsdone from '../../FetchFromWhatsdone';

const containerState = {};

const mapStateToProps = (state, ownProps) => {
  containerState.ownProps = ownProps;
  return {};
};

const mapDispatchToProps = _dispatch => {
  return {
    onSubmit: loginDetails => {
      login(loginDetails)
        .then(() => {
          containerState.ownProps.history.push('/');
        })
        .catch(e => {
          console.error(e.stack);   // eslint-disable-line no-console
        });
    }
  };
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

const SigninPageContainer = connect(mapStateToProps, mapDispatchToProps)(SigninPage);

export default SigninPageContainer;
