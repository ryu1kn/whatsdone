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
        .then(function (response) {
          dispatch(Action.markPostDoneSuccess(response.body));
        }).catch(e => {
          dispatch(Action.markPostDoneFailed(e));
        });
    }
  };
};

export {mapDispatchToProps};
