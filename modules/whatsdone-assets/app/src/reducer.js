
import {combineReducers} from 'redux';

export default combineReducers({
  done: require('./done/reducer'),
  login: require('./signin/reducer')
});
