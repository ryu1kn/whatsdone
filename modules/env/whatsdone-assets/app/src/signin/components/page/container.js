
import Action from '../../action';
import ServiceLocator from '../../../service-locator';

const containerState = {};

const mapStateToProps = (state, ownProps) => {
  containerState.ownProps = ownProps;
  return {};
};

const mapDispatchToProps = dispatch => {
  return {
    onSubmit: loginDetails => {
      dispatch(Action.login(loginDetails, dispatch));
      ServiceLocator.whatsdoneApiClient.login(loginDetails)
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
