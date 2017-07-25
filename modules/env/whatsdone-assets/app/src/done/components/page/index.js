
import './style.less';

import {connect} from 'react-redux';
import Component from './component';
import {mapStateToProps, mapDispatchToProps} from './container';

export default connect(mapStateToProps, mapDispatchToProps)(Component);
