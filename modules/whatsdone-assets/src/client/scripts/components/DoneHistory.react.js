
var _ = require('lodash');
var React = require('react');

var DoneList = require('./DoneList.react');

var DoneHistory = React.createClass({

  ONEDAY_MS: 24 * 60 * 60 * 1000,

  /**
   * @param {Date} d
   * @return {string} "yyyy-mm-dd" (local time)
   */
  getLocalDateString: function (d) {
    let month = d.getMonth() + 1;
    let date = d.getDate();
    month = month < 10 ? `0${month}` : month;
    date = date < 10 ? `0${date}` : date;

    return `${d.getFullYear()}-${month}-${date}`;
  },

  /**
   * @param {Date} date
   * @return {string}
   */
  getDayLabel: function (date) {
    return date.toLocaleDateString('en-GB', {
      weekday: 'short',
      year: 'numeric',
      month: 'long',
      day: 'numeric'
    });
  },

  /**
   * @param {string} dateString "yyyy-mm-dd"
   * @return {string}
   */
  getFriendlyDayLabel: function (dateString) {
    const now = new Date();
    const today = this.getLocalDateString(now);
    const yesterday = this.getLocalDateString(new Date(now - this.ONEDAY_MS));
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
    var grouped = _.groupBy(this.props.data, entry =>
                      this.getLocalDateString(entry.date));
    return (
      <div className="donehistory">
        {_.sortBy(_.toPairs(grouped), pair => pair[0]).reverse()
            .map((data, index) =>
              <DoneList title={this.getFriendlyDayLabel(data[0])} data={data[1]} key={index} />
        )}
      </div>
    );
  }

});

module.exports = DoneHistory;
