import React from 'react';

import DoneItem from './done-item';

class DoneList extends React.Component {

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
