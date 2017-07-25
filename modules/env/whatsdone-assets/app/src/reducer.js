
import {combineReducers} from 'redux';

export default combineReducers({
  dones: require('./done/reducer'),
  loginStatus: require('./signin/reducer')
});
