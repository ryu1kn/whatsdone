/* global Showdown */

import React from 'react';
import ReactDOM from 'react-dom';
import util from '../../../util';
import DoneAction from '../Actions';

const converter = new Showdown.converter(); // eslint-disable-line new-cap

class DoneItem extends React.Component {

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
    return name ? util.getColorCode(name) : '#DDDDDD';
  }

  delete(e) {
    e.stopPropagation();
    ReactDOM.findDOMNode(this).setAttribute('style', 'display:none;');
    DoneAction.destroy(this.props.doneId);
  }

  render() {
    var rawMarkup = converter.makeHtml(this.props.children.toString());
    return (
      <div className="doneitem">
        <div className="doneitem__user">
          <div className="doneitem__user-icon"
               style={{backgroundColor: this.getIconColor(this.props.username)}}>
            {this.getFirstLetter(this.props.username)}
          </div>
          <div className="doneiten__user-name">{this.props.username}</div>
        </div>
        <div>
          <div className="doneitem__done-thing"
               dangerouslySetInnerHTML={{__html: rawMarkup}} />
          <p className="doneitem__time">
            {this.formatTime(this.props.date)}
          </p>
        </div>
        <div className="doneitem__delete-action glyphicon glyphicon-remove"
             onClick={this.delete.bind(this)}></div>
      </div>
    );
  }

}

module.exports = DoneItem;
