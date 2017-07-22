
// XXX: Rename this to DoneApp. This component is a controller-view
import './style.less';

import React from 'react';
import {Link} from 'react-router-dom';
import DoneStore from './Store';

import DoneHistory from './components/History';
import DoneForm from './components/Form';

class DonePage extends React.Component {

  constructor(params) {
    super(params);

    this.state = {data: []};
    DoneStore.load();
  }

  componentDidMount() {
    DoneStore.addChangeListener(this._onChange.bind(this));
  }

  componentWillUnmount() {
    DoneStore.removeChangeListener(this._onChange.bind(this));
  }

  _onChange() {
    this.setState({data: DoneStore.getAll()});
  }

  render() {
    return (
      <div className="donebox container">
        <Link to="/signin">To Sign in</Link>
        <h2 className="donebox-title page-header">What's Done?</h2>
        <DoneForm />
        <DoneHistory data={this.state.data} />
      </div>
    );
  }

}

module.exports = DonePage;
