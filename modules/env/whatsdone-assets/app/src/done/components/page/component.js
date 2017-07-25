
import './style.less';

import React from 'react';
import {Link} from 'react-router-dom';

import DoneHistory from '../History';
import DoneForm from '../form';

class DonePage extends React.Component {

  render() {
    return (
      <div className="donebox container">
        <Link to="/signin">To Sign in</Link>
        <h2 className="donebox-title page-header">What's Done?</h2>
        <DoneForm />
        <DoneHistory data={this.props.dones} />
      </div>
    );
  }

}

module.exports = DonePage;
