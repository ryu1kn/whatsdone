
import Action from '../../action';
import ServiceLocator from '../../../service-locator';

const containerState = {};

const mapStateToProps = (state, ownProps) => {
  containerState.ownProps = ownProps;
  return {done: state.done};
};

const mapDispatchToProps = dispatch => {
  return {
    fetchDones: () => {
      dispatch(Action.getDones());
      ServiceLocator.whatsdoneApiClient.getDones()
        .then(response => {
          switch (response.status) {
          case 200: {
            const body = response.body;
            const dones = body.items || body;
            return dispatch(Action.markGetDonesSuccess(dones, body.nextKey));
          }
          case 401:
            return containerState.ownProps.history.push('/signin');
          default:
            throw new Error(`Unexpected response code ${response.status}`);
          }
        }).catch(e => {
          dispatch(Action.markGetDonesFailed(e));
        });
    }
  };
};

export {mapStateToProps, mapDispatchToProps};
