
import Action from '../../action';
import ServiceLocator from '../../../service-locator';

const mapDispatchToProps = dispatch => {
  return {
    onSubmit: doneThing => {
      const doneItem = {
        doneThing,
        date: new Date().toISOString()
      };
      dispatch(Action.postDone(doneItem));
      ServiceLocator.whatsdoneApiClient.postDone(doneItem)
        .then(function (updatedItem) {
          dispatch(Action.markPostDoneSuccess(updatedItem));
        }).catch(e => {
          dispatch(Action.markPostDoneFailed(e));
        });
    }
  };
};

export {mapDispatchToProps};
