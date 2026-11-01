import React from 'react';
import type {NextKey} from '../../reducer';

interface DoneLoaderProps {
  fetchDones: (nextKey: NextKey) => void
  nextKey: NextKey
}

export class DoneLoader extends React.Component<DoneLoaderProps> {

  private fetchDones() {
    this.props.fetchDones(this.props.nextKey);
  }

  render() {
    return (
      <div className="card">
        <div className="card-body" onClick={this.fetchDones.bind(this)}>
          Tap to load more
        </div>
      </div>
    );
  }
}
