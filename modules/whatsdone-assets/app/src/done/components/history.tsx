import _ from 'lodash';
import React from 'react';

import DoneList from './done-list';
import DoneLoader from './done-loader';
import {DoneItem, DoneState} from '../reducer';
import {getFriendlyDayLabel, getLocalDateString} from './date-util';

class DoneHistory extends React.Component<{done: DoneState}> {

  render() {
    const grouped = _.groupBy(
      this.props.done.items,
      (entry: DoneItem) => getLocalDateString(entry.date)
    );
    return (
      <div className="donehistory">
        {_.sortBy(_.toPairs(grouped), pair => pair[0]).reverse()
          .map((data, index) =>
            <DoneList title={getFriendlyDayLabel(data[0])} data={data[1]} key={index} />
          )}
        {this.props.done.nextKey ? <DoneLoader nextKey={this.props.done.nextKey} /> : ''}
      </div>
    );
  }

}

export default DoneHistory;
