
// XXX: Rename this to DoneApp. This component is a controller-view

import React from 'react';
import {Link} from 'react-router-dom';
import DoneStore from '../stores/DoneStore';

import DoneHistory from './DoneHistory.react';
import DoneForm from './DoneForm.react';

/**
 * Retrieve the current DONE data from the DoneStore
 */
function getDoneStore() {
  return {
    data: DoneStore.getAll()
  };
}

var DoneBox = React.createClass({

  getInitialState: function () {
    DoneStore.load();
    return getDoneStore();
  },

  componentDidMount: function () {
    DoneStore.addChangeListener(this._onChange);
  },

  componentWillUnmount: function () {
    DoneStore.removeChangeListener(this._onChange);
  },

  _onChange: function () {
    this.setState(getDoneStore());
  },

  render: function () {
    return (
      <div className="donebox container">
        <Link to="/signin">To Sign in</Link>
        <h2 className="donebox-title page-header">What's Done?</h2>
        <DoneForm />
        <DoneHistory data={this.state.data} />
      </div>
    );
  }

});

module.exports = DoneBox;
