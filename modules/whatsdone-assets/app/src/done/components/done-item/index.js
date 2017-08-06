
import {connect} from 'react-redux';
import Component from './component';
import {mapDispatchToProps} from './container';

export default connect(null, mapDispatchToProps)(Component);
