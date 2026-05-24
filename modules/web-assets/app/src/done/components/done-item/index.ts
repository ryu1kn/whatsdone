import {connect} from 'react-redux';
import {DoneItem} from './component';
import {mapDispatchToProps} from './container';

export default connect(null, mapDispatchToProps)(DoneItem);
