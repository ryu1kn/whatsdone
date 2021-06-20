import React from 'react';

import DoneItem from './done-item';

// TODO: Can this go to non-UI file?
interface DoneItemData {
  id: string
  date: Date
  username: string
  doneThing: string
}

export interface DoneListProps {
  title: string
  data: DoneItemData[]
}

class DoneList extends React.Component<DoneListProps> {

  render() {
    const doneItemNodes = this.props.data.map(function (doneItem, index) {
      return (
        <DoneItem doneId={doneItem.id} date={doneItem.date}
          username={doneItem.username} key={index}>
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
