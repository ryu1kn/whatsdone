import './style.less';

import {connect} from 'react-redux';
import {DonePage} from './component';
import {mapDispatchToProps, mapStateToProps} from './container';

export default connect(mapStateToProps, mapDispatchToProps)(DonePage);
