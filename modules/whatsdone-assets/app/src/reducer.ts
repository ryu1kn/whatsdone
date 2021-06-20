import {combineReducers} from 'redux';
import reducer, {DoneState} from './done/reducer';

export interface RootState {
  done: DoneState
}

export default combineReducers({done: reducer});
