import React from 'react';
import ReactDOM from 'react-dom';
import {getColorCode} from '../../../util';
import {formatTime} from '../date-util';
import {Converter} from 'showdown';

const converter = new Converter();

interface DoneItemProps {
  doneId: string
  date: Date
  username: string
  deleteDone: (id: string) => void
  children: string
}

const getFirstLetter = (name: string) => (name || '').charAt(0).toUpperCase() || '?';

const getIconColor = (name: string) => name ? getColorCode(name) : '#DDDDDD';

export class DoneItem extends React.Component<DoneItemProps> {

  private delete(e: any) {
    e.stopPropagation();
    (ReactDOM.findDOMNode(this) as Element).setAttribute('style', 'display:none;');
    this.props.deleteDone(this.props.doneId);
  }

  render() {
    const rawMarkup = converter.makeHtml(this.props.children.toString());
    return (
      <div className="doneitem">
        <div className="doneitem__user">
          <div className="doneitem__user-icon" style={{backgroundColor: getIconColor(this.props.username)}}>
            {getFirstLetter(this.props.username)}
          </div>
          <div className="doneiten__user-name">{this.props.username}</div>
        </div>
        <div>
          <div className="doneitem__done-thing" dangerouslySetInnerHTML={{__html: rawMarkup}} />
          <p className="doneitem__time">
            {formatTime(this.props.date)}
          </p>
        </div>
        <div className="doneitem__delete-action glyphicon glyphicon-trash" onClick={this.delete.bind(this)}></div>
      </div>
    );
  }
}
