/* global Showdown */
const React = require('react');
const ReactDOM = require('react-dom');
const util = require('../util');
const DoneAction = require('../actions/DoneAction');

const converter = new Showdown.converter(); // eslint-disable-line new-cap

var DoneItem = React.createClass({

  formatTime: function (date) {
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
  },

  getFirstLetter: function (name) {
    return (name || '').charAt(0).toUpperCase() || '?';
  },

  getIconColor: function (name) {
    return name ? util.getColorCode(name) : '#DDDDDD';
  },

  /**
   * @param {Event} e
   */
  delete: function (e) {
    e.stopPropagation();
    ReactDOM.findDOMNode(this).setAttribute('style', 'display:none;');
    DoneAction.destroy(this.props.doneId);
  },

  render: function () {
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
             onClick={this.delete}></div>
      </div>
    );
  }
});

module.exports = DoneItem;
