import {combineReducers} from 'redux';
import reducer from './done/reducer';

export default combineReducers({done: reducer});
