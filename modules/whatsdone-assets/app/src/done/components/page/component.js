
import './style.less';

import React from 'react';
import {Link} from 'react-router-dom';

import DoneHistory from '../history';
import DoneForm from '../form';

class DonePage extends React.Component {

  constructor(params) {
    super(params);

    const fetchDones = () => {
      if (this.props.done.apiReady) {
        this.props.fetchDones();
      } else {
        setTimeout(fetchDones, 100);
      }
    };
    fetchDones();
  }

  render() {
    return (
      <div className="donebox container">
        <Link to="/signin">To Sign in</Link>
        <h2 className="donebox-title page-header">What's Done?</h2>
        <DoneForm />
        <DoneHistory done={this.props.done} />
      </div>
    );
  }

}

module.exports = DonePage;
