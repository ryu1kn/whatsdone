import {connect} from 'react-redux';
import DoneItem from './DoneItem';
import {requestDeleteDone, succeesDeleteDone, failedDeleteDone} from '../../actions';
import ServiceLocator from '../../../ServiceLocator';

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

const DoneItemContainer = connect(null, mapDispatchToProps)(DoneItem);

export default DoneItemContainer;
