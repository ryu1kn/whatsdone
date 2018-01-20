
import Action from '../../action';
import ServiceLocator from '../../../service-locator';

const containerState = {};

const mapStateToProps = (state, ownProps) => {
  const loginState = state.loginStatus;
  Object.assign(containerState, {
    ownProps,
    cognitoUser: loginState.cognitoUser
  });
  return {status: loginState.status};
};

const mapDispatchToProps = dispatch => {
  return {
    onSubmit: loginDetails => {
      dispatch(Action.login(loginDetails, dispatch));
      ServiceLocator.authenticator.authenticate(loginDetails)
        .then(({newPasswordRequired, cognitoUser}) => {
          if (newPasswordRequired) {
            dispatch(Action.markNewPasswordRequired(cognitoUser));
            return;
          }
          dispatch(Action.markLoginSuccess());
          containerState.ownProps.history.push('/');
        })
        .catch(e => {
          dispatch(Action.markLoginFailed(e));
        });
    },
    onSubmitNewPassword: newPassword => {
      ServiceLocator.authenticator.updatePassword(containerState.cognitoUser, newPassword)
        .then(() => {
          dispatch(Action.markLoginSuccess());
          containerState.ownProps.history.push('/');
        })
        .catch(e => {
          dispatch(Action.markLoginFailed(e));
        });
    }
  };
};

export {mapStateToProps, mapDispatchToProps};
