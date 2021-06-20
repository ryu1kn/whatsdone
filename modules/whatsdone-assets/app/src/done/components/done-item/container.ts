import Action from '../../action';
import ServiceLocator from '../../../service-locator';
import {Dispatch} from 'redux';

const mapDispatchToProps = (dispatch: Dispatch) => {
  return {
    deleteDone: (id: string) => {
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
