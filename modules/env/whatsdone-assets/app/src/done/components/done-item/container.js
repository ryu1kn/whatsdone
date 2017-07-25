
import {requestDeleteDone, succeesDeleteDone, failedDeleteDone} from '../../actions';
import ServiceLocator from '../../../service-locator';

const mapDispatchToProps = dispatch => {
  return {
    deleteDone: id => {
      dispatch(requestDeleteDone(id));
      ServiceLocator.whatsdoneApiClient.deleteDone(id)
        .then(() => {
          dispatch(succeesDeleteDone(id));
        }).catch(e => {
          dispatch(failedDeleteDone(e));
        });
    }
  };
};

export {mapDispatchToProps};
