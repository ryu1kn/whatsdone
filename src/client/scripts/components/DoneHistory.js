
var _ = require('lodash');
var React = require('react');

var DoneList = require('./DoneList');

var DoneHistory = React.createClass({

  ONEDAY_MS: 24 * 60 * 60 * 1000,
  
  DATE_STRING_LEN: 'yyyy-mm-dd'.length,

  /**
   * @param {Date}
   * @return {string} "yyyy-mm-dd"
   */
  getISODateString: function (date) {
    return date.toISOString().substr(0, this.DATE_STRING_LEN);
  },

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
   * @param {string} dateString "yyyy-mm-dd"
   * @return {string}
   */
  getFriendlyDayLabel: function (dateString) {
    var now = new Date(),
        today = this.getISODateString(now),
        yesterday = this.getISODateString(new Date(now - this.ONEDAY_MS));
    switch (dateString) {
    case today:
      return 'Today';
    case yesterday:
      return 'Yesterday';
    default:
      return this.getDayLabel(new Date(dateString));
    }
  },

  render: function () {
    var grouped = _.groupBy(this.props.data, (entry) =>
                      this.getISODateString(entry.date));
    return (
      <div className="donehistory">
        {_.sortBy(_.pairs(grouped), (pair) => pair[0]).reverse()
            .map((data) =>
              <DoneList title={this.getFriendlyDayLabel(data[0])} data={data[1]} />
        )}
      </div>
    );
  }

});

module.exports = DoneHistory;
