
import Action from '../../action';
import ServiceLocator from '../../../service-locator';

const mapStateToProps = state => ({dones: state.dones});

const mapDispatchToProps = dispatch => {
  return {
    fetchDones: () => {
      dispatch(Action.getDones());
      ServiceLocator.whatsdoneApiClient.getDones()
        .then(response => {
          dispatch(Action.markGetDonesSuccess(response));
        }).catch(e => {
          dispatch(Action.markGetDonesFailed(e));
        });
    }
  };
};

export {mapStateToProps, mapDispatchToProps};
