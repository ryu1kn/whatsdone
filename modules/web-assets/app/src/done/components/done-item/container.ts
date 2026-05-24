import Action from '../../action';
import ServiceLocator from '../../../service-locator';
import {Dispatch} from 'redux';

const mapDispatchToProps = (dispatch: Dispatch) => {
  const whatsdoneApiClient = ServiceLocator.whatsdoneApiClient;
  return {
    deleteDone: (id: string) => {
      dispatch(Action.deleteDone(id));
      whatsdoneApiClient.deleteDone(id)
        .then(() => {
          dispatch(Action.markDeleteDoneSuccess(id));
        }).catch(e => {
          dispatch(Action.markDeleteDoneFailed(e));
        });
    },
    updateDone: (id: string, doneThing: string) => {
      dispatch(Action.updateDone(id, doneThing));
      whatsdoneApiClient.updateDone(id, doneThing)
        .then(() => {
          dispatch(Action.markUpdateDoneSuccess(id));
        }).catch(e => {
          dispatch(Action.markUpdateDoneFailed(e));
        });
    },
    startEditDone: (id: string) => {
      dispatch(Action.startEditDone(id));
    }
  };
};

export {mapDispatchToProps};
