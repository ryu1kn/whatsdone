import React from 'react';

export class DoneLoader extends React.Component {

  fetchDones() {
    this.props.fetchDones(this.props.nextKey);
  }

  render() {
    return (
      <div className="panel panel-default">
        <div className="panel-body" onClick={this.fetchDones.bind(this)}>
          Tap to load more
        </div>
      </div>
    );
  }

}
