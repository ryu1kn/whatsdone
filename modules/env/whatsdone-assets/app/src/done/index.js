import {connect} from 'react-redux';
import DonePage from './Page';

const mapStateToProps = state => ({dones: state.dones});

const DonePageContainer = connect(mapStateToProps)(DonePage);

export default DonePageContainer;
