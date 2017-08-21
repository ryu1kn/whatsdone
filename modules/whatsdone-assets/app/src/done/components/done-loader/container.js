
import Action from '../../action';
import ServiceLocator from '../../../service-locator';

const containerState = {};

const mapStateToProps = state => {
  return {nextKey: state.done.nextKey};
};

const mapDispatchToProps = dispatch => {
  return {
    fetchDones: nextKey => {
      dispatch(Action.getDones());
      ServiceLocator.whatsdoneApiClient.getDones(nextKey)
        .then(response => {
          switch (response.status) {
          case 200: {
            const body = response.body;
            const dones = body.items || body;
            return dispatch(Action.markGetDonesSuccess(dones, body.nextKey));
          }
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
