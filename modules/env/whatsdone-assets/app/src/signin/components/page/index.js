
import {connect} from 'react-redux';
import Component from './component';
import * as container from './container';

export default connect(container.mapStateToProps, container.mapDispatchToProps)(Component);
