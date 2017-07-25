
import {requestLogin, successLogin, failedLogin} from '../../actions';
import ServiceLocator from '../../../ServiceLocator';

const containerState = {};

const mapStateToProps = (state, ownProps) => {
  containerState.ownProps = ownProps;
  return {};
};

const mapDispatchToProps = dispatch => {
  return {
    onSubmit: loginDetails => {
      dispatch(requestLogin(loginDetails, dispatch));
      ServiceLocator.whatsdoneApiClient.login(loginDetails)
        .then(() => {
          dispatch(successLogin());
          containerState.ownProps.history.push('/');
        })
        .catch(e => {
          dispatch(failedLogin(e));
        });
    }
  };
};

export {mapStateToProps, mapDispatchToProps};
