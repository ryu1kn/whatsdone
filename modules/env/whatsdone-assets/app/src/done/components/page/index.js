
import './style.less';

import {connect} from 'react-redux';
import Component from './component';
import {mapStateToProps} from './container';

export default connect(mapStateToProps)(Component);
