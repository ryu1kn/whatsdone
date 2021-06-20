import React from 'react';
import ReactDOM from 'react-dom';
import {getColorCode} from '../../../util';

const showdown = require('showdown');
const converter = new showdown.Converter();

interface DoneItemProps {
  doneId: string
  date: Date
  username: string
  deleteDone: (id: string) => void
  children: string
}

export class DoneItem extends React.Component<DoneItemProps> {

  formatTime(date) {
    let hour = date.getHours();
    let mins = date.getMinutes();
    const ampm = hour < 12 ? 'am' : 'pm';

    if (hour > 12) {
      hour -= 12;
    }
    if (mins < 10) {
      mins = '0' + mins;
    }

    return `${hour}:${mins} ${ampm}`;
  }

  getFirstLetter(name) {
    return (name || '').charAt(0).toUpperCase() || '?';
  }

  getIconColor(name) {
    return name ? getColorCode(name) : '#DDDDDD';
  }

  delete(e) {
    e.stopPropagation();
    (ReactDOM.findDOMNode(this) as Element).setAttribute('style', 'display:none;');
    this.props.deleteDone(this.props.doneId);
  }

  render() {
    var rawMarkup = converter.makeHtml(this.props.children.toString());
    return (
      <div className="doneitem">
        <div className="doneitem__user">
          <div className="doneitem__user-icon" style={{backgroundColor: this.getIconColor(this.props.username)}}>
            {this.getFirstLetter(this.props.username)}
          </div>
          <div className="doneiten__user-name">{this.props.username}</div>
        </div>
        <div>
          <div className="doneitem__done-thing" dangerouslySetInnerHTML={{__html: rawMarkup}} />
          <p className="doneitem__time">
            {this.formatTime(this.props.date)}
          </p>
        </div>
        <div className="doneitem__delete-action glyphicon glyphicon-trash" onClick={this.delete.bind(this)}></div>
      </div>
    );
  }

}
