import {connect} from 'react-redux';
import {DoneLoader} from './component';
import {mapDispatchToProps} from './container';

export default connect(null, mapDispatchToProps)(DoneLoader);
