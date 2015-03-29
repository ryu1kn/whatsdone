
var _ = require('lodash');
var React = require('react');

var DoneList = require('./DoneList');

var DoneHistory = React.createClass({
  getDayLabel: function (dateString) {
    var now = new Date(),
        today = now.toISOString().substr(0, 10),
        yesterday = (new Date(now - 24 * 60 * 60 * 1000)).toISOString().substr(0, 10);
    switch (dateString) {
    case today:
      return 'Today';
    case yesterday:
      return 'Yesterday';
    default:
      return dateString;
    }
  },
  render: function() {
    var grouped = _.groupBy(this.props.data, (entry) =>
                      entry.date.substr(0, 'yyyy-mm-dd'.length));
    return (
      <div className="donehistory">
        {_.pairs(grouped).map((data) =>
          <DoneList title={this.getDayLabel(data[0])} data={data[1]} />
        )}
      </div>
    );
  }
});

module.exports = DoneHistory;
