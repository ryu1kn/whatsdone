
var _ = require('lodash');
var React = require('react');

var DoneList = require('./DoneList');

var DoneHistory = React.createClass({

  OneDay_ms: 24 * 60 * 60 * 1000,

  /**
   * @param {Date} date
   * @return {string}
   */
  getDayLabel: function (date) {
    return date.toLocaleDateString('en-GB', {
      weekday: 'short',
      year   : 'numeric',
      month  : 'long',
      day    : 'numeric'
    });
  },

  /**
   * @param {string} dateString
   * @return {string}
   */
  getFriendlyDayLabel: function (dateString) {
    var now = new Date(),
        today = this.getDayLabel(now),
        yesterday = this.getDayLabel(new Date(now - this.OneDay_ms));
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
                      this.getDayLabel(new Date(entry.date)));
    return (
      <div className="donehistory">
        {_.pairs(grouped).map((data) =>
          <DoneList title={this.getFriendlyDayLabel(data[0])} data={data[1]} />
        )}
      </div>
    );
  }

});

module.exports = DoneHistory;
