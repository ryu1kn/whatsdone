
import _ from 'lodash';
import React from 'react';

import DoneList from './DoneList';

const ONEDAY_MS = 24 * 60 * 60 * 1000;

class DoneHistory extends React.Component {

  getLocalDateString(d) {
    let month = d.getMonth() + 1;
    let date = d.getDate();
    month = month < 10 ? `0${month}` : month;
    date = date < 10 ? `0${date}` : date;

    return `${d.getFullYear()}-${month}-${date}`;
  }

  getDayLabel(date) {
    return date.toLocaleDateString('en-GB', {
      weekday: 'short',
      year: 'numeric',
      month: 'long',
      day: 'numeric'
    });
  }

  getFriendlyDayLabel(dateString) {
    const now = new Date();
    const today = this.getLocalDateString(now);
    const yesterday = this.getLocalDateString(new Date(now - ONEDAY_MS));
    switch (dateString) {
    case today:
      return 'Today';
    case yesterday:
      return 'Yesterday';
    default:
      return this.getDayLabel(new Date(dateString));
    }
  }

  render() {
    const grouped = _.groupBy(this.props.data, entry =>
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

}

module.exports = DoneHistory;
