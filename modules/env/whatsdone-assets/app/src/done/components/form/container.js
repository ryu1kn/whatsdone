
import {requestPostDone, succeesPostDone, failedPostDone} from '../../actions';
import ServiceLocator from '../../../service-locator';

const mapDispatchToProps = dispatch => {
  return {
    onSubmit: doneThing => {
      const doneItem = {
        doneThing,
        date: new Date().toISOString()
      };
      dispatch(requestPostDone(doneItem));
      ServiceLocator.whatsdoneApiClient.postDone(doneItem)
        .then(function (updatedItem) {
          dispatch(succeesPostDone(updatedItem));
        }).catch(e => {
          dispatch(failedPostDone(e));
        });
    }
  };
};

export {mapDispatchToProps};
