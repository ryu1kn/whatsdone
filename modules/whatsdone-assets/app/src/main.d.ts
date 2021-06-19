import {compose} from 'redux';

declare global {
  interface Window {__REDUX_DEVTOOLS_EXTENSION__?: typeof compose;}
}
