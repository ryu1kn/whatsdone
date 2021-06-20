import Action from '../../action';
import ServiceLocator from '../../../service-locator';
import {Dispatch} from 'redux';

const mapDispatchToProps = (dispatch: Dispatch) => {
  return {
    onSubmit: (doneThing: string) => {
      const doneItem = {
        doneThing,
        date: new Date().toISOString()
      };
      dispatch(Action.postDone(doneItem));
      ServiceLocator.whatsdoneApiClient.postDone(doneItem)
        .then(function (response) {
          dispatch(Action.markPostDoneSuccess(response.body));
        }).catch(e => {
          dispatch(Action.markPostDoneFailed(e));
        });
    }
  };
};

export {mapDispatchToProps};
