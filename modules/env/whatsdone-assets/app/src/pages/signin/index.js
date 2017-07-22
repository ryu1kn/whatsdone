import {connect} from 'react-redux';
import SigninPage from './page';
import apiClient from '../../whatsdoneApiClient';

const containerState = {};

const mapStateToProps = (state, ownProps) => {
  containerState.ownProps = ownProps;
  return {};
};

const mapDispatchToProps = _dispatch => {
  return {
    onSubmit: loginDetails => {
      apiClient.login(loginDetails)
        .then(() => {
          containerState.ownProps.history.push('/');
        })
        .catch(e => {
          console.error(e.stack);   // eslint-disable-line no-console
        });
    }
  };
};

const SigninPageContainer = connect(mapStateToProps, mapDispatchToProps)(SigninPage);

export default SigninPageContainer;