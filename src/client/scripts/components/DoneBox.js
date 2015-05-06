
var $ = require('jquery');
var React = require('react');

var DoneHistory = require('./DoneHistory');
var DoneForm = require('./DoneForm');

var DoneBox = React.createClass({

  loadDoneItemsFromServer: function () {
    $.ajax({
      url: this.props.url,
      dataType: 'json',
      success: function(data) {
        this.setState({data: data});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error(this.props.url, status, err.toString());
      }.bind(this)
    });
  },

  handleDoneItemSubmit: function (doneItem) {
    doneItem.date = new Date().toISOString();
    var doneItems = this.state.data;
    doneItems.push(doneItem);
    this.setState({data: doneItems}, function () {
      $.ajax({
        url: this.props.url,
        dataType: 'json',
        type: 'POST',
        data: doneItem,
        success: function(data) {
          this.setState({data: data});
        }.bind(this),
        error: function(xhr, status, err) {
          console.error(this.props.url, status, err.toString());
        }.bind(this)
      });
    });
  },

  getInitialState: function () {
    return {data: []};
  },

  componentDidMount: function () {
    this.loadDoneItemsFromServer();
  },
  
  /**
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
        <DoneForm onDoneItemSubmit={this.handleDoneItemSubmit} />
        <DoneHistory data={this.preprocessData(this.state.data)} />
      </div>
    );
  }
});

module.exports = DoneBox;
