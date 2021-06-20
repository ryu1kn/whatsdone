import Action from '../../action';
import ServiceLocator from '../../../service-locator';
import {Dispatch} from 'redux';
import {NextKey} from '../../reducer';

const mapDispatchToProps = (dispatch: Dispatch) => {
  return {
    fetchDones: (nextKey?: NextKey) => {
      dispatch(Action.getDones());
      ServiceLocator.whatsdoneApiClient.getDones(nextKey)
        .then(response => {
          switch (response.status) {
          case 200: {
            const body = response.body;
            const dones = body.items || body;
            return dispatch(Action.markGetDonesSuccess(dones, body.nextKey));
          }
          default:
            throw new Error(`Unexpected response code ${response.status}`);
          }
        }).catch(e => {
          dispatch(Action.markGetDonesFailed(e));
        });
    }
  };
};

export {mapDispatchToProps};
