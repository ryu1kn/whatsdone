
// XXX: Rename this to DoneApp. This component is a controller-view

var $ = require('jquery');
var _ = require('lodash');
var React = require('react');
var DoneStore = require('../stores/DoneStore');

var DoneHistory = require('./DoneHistory.react');
var DoneForm = require('./DoneForm.react');


/**
 * Retrieve the current DONE data from the DoneStore
 */
function getDoneStore() {
  return {
    data: DoneStore.getAll()
  };
}

var DoneBox = React.createClass({

  getInitialState: function() {
    DoneStore.load();
    return getDoneStore();
  },

  componentDidMount: function() {
    DoneStore.addChangeListener(this._onChange);
  },

  componentWillUnmount: function() {
    DoneStore.removeChangeListener(this._onChange);
  },

  _onChange: function() {
    this.setState(getDoneStore());
  },

  render: function () {
    return (
      <div className="donebox container">
        <h2 className="donebox-title page-header">What's Done?</h2>
        <DoneForm />
        <DoneHistory data={this.state.data} />
      </div>
    );
  }

});

module.exports = DoneBox;
