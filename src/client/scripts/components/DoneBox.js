
// XXX: Rename this to DoneApp. This component is a controller-view

var $ = require('jquery');
var _ = require('lodash');
var React = require('react');
var DoneStore = require('../stores/DoneStore');

var DoneHistory = require('./DoneHistory');
var DoneForm = require('./DoneForm');


/**
 * Retrieve the current DONE data from the DoneStore
 */
function getDoneStore() {
  return {
    // TODO: sort should be moved to DoneStore
    data: DoneStore.getAll().sort(function (a, b) {
      return a.date < b.date ?  1 :
             a.date > b.date ? -1 : 0;
    })
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

  /**
   * TODO: should be moved to DoneStore
   * @private
   * @param {Array.<Object>} dones list of done data
   * @return {Array.<Object>}
   */
  preprocessData: function (dones) {
    return dones.map((done) => {
      done.date = new Date(done.date);
      return done;
    });
  },

  render: function () {
    return (
      <div className="donebox container">
        <h2 className="donebox-title page-header">What's Done?</h2>
        <DoneForm />
        <DoneHistory data={this.preprocessData(this.state.data)} />
      </div>
    );
  }

});

module.exports = DoneBox;
