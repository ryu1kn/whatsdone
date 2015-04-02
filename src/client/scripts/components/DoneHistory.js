
var _ = require('lodash');
var React = require('react');

var DoneList = require('./DoneList');

var DoneHistory = React.createClass({

  oneDay_ms: 24 * 60 * 60 * 1000,

  getDayLabel: function (dateString) {
    var now = new Date(),
        today = now.toLocaleDateString(),
        yesterday = (new Date(now - this.oneDay_ms)).toLocaleDateString();
    switch (dateString) {
    case today:
      return 'Today';
    case yesterday:
      return 'Yesterday';
    default:
      return dateString;
    }
  },

  render: function () {
    var grouped = _.groupBy(this.props.data, (entry) =>
                      new Date(entry.date).toLocaleDateString());
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
