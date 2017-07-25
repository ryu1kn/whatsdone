
import Action from '../../action';
import ServiceLocator from '../../../service-locator';

const mapDispatchToProps = dispatch => {
  return {
    deleteDone: id => {
      dispatch(Action.deleteDone(id));
      ServiceLocator.whatsdoneApiClient.deleteDone(id)
        .then(() => {
          dispatch(Action.markDeleteDoneSuccess(id));
        }).catch(e => {
          dispatch(Action.markDeleteDoneFailed(e));
        });
    }
  };
};

export {mapDispatchToProps};
