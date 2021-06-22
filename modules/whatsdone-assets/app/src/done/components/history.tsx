import _ from 'lodash';
import React from 'react';

import DoneList from './done-list';
import DoneLoader from './done-loader';
import {DoneItem, DoneState} from '../reducer';
import {getLocalDateString} from './date-util';

const ONEDAY_MS = 24 * 60 * 60 * 1000;

class DoneHistory extends React.Component<{done: DoneState}> {

  getDayLabel(date: Date) {
    return date.toLocaleDateString('en-GB', {
      weekday: 'short',
      year: 'numeric',
      month: 'long',
      day: 'numeric'
    });
  }

  getFriendlyDayLabel(dateString: string) {
    const now = new Date();
    const today = getLocalDateString(now);
    const yesterday = getLocalDateString(new Date(now.getTime() - ONEDAY_MS));
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
    const grouped = _.groupBy(
      this.props.done.items,
      (entry: DoneItem) => getLocalDateString(entry.date)
    );
    return (
      <div className="donehistory">
        {_.sortBy(_.toPairs(grouped), pair => pair[0]).reverse()
          .map((data, index) =>
            <DoneList title={this.getFriendlyDayLabel(data[0])} data={data[1]} key={index} />
          )}
        {this.props.done.nextKey ? <DoneLoader nextKey={this.props.done.nextKey} /> : ''}
      </div>
    );
  }

}

export default DoneHistory;
