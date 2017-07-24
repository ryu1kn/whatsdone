import {connect} from 'react-redux';
import InputForm from './Form';
import {requestPostDone, succeesPostDone, failedPostDone} from '../../actions';
import ServiceLocator from '../../../ServiceLocator';

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

const InputFormContainer = connect(null, mapDispatchToProps)(InputForm);

export default InputFormContainer;
