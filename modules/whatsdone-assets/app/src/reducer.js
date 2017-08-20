
import {combineReducers} from 'redux';

export default combineReducers({
  done: require('./done/reducer'),
  loginStatus: require('./signin/reducer')
});
