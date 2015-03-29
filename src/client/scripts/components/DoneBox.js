
var $ = require('jquery');
var React = require('react');

var DoneHistory = require('./DoneHistory');
var DoneForm = require('./DoneForm');

var DoneBox = React.createClass({
  loadDoneItemsFromServer: function() {
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
  handleDoneItemSubmit: function(doneItem) {
    doneItem.date = new Date().toISOString();
    var doneItems = this.state.data;
    doneItems.push(doneItem);
    this.setState({data: doneItems}, function() {
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
  getInitialState: function() {
    return {data: []};
  },
  componentDidMount: function() {
    this.loadDoneItemsFromServer();
    // setInterval(this.loadDoneItemsFromServer, this.props.pollInterval);
  },
  render: function() {
    return (
      <div className="donebox">
        <DoneForm onDoneItemSubmit={this.handleDoneItemSubmit} />
        <DoneHistory data={this.state.data.reverse()} />
      </div>
    );
  }
});

module.exports = DoneBox;
