import React from 'react';

import DoneItem from './done-item';
import {DoneItem as DoneItemData} from '../reducer';

export interface DoneListProps {
  title: string
  data: DoneItemData[]
}

class DoneList extends React.Component<DoneListProps> {

  render() {
    const doneItemNodes = this.props.data.map(function (doneItem, index) {
      return (
        <DoneItem doneId={doneItem.id} date={doneItem.date}
          username={doneItem.username} key={index} editInProgress={doneItem.editInProgress}>
          {doneItem.doneThing}
        </DoneItem>
      );
    });
    return (
      <div className="donelist">
        <h3 className="donelist__title">{this.props.title}</h3>
        {doneItemNodes}
      </div>
    );
  }

}

export default DoneList;
