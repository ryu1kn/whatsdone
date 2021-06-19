import {connect} from 'react-redux';
import {DoneForm} from './component';
import {mapDispatchToProps} from './container';

export default connect(null, mapDispatchToProps)(DoneForm);
