import {combineReducers} from 'redux';
import reducer from './done/reducer';
import type {DoneState} from './done/reducer';

export interface RootState {
  done: DoneState
}

export default combineReducers({done: reducer});
