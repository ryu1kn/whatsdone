import React from 'react';

interface DoneLoaderProps {
  fetchDones: (nextKey: any) => void
  nextKey: any
}

export class DoneLoader extends React.Component<DoneLoaderProps> {

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
