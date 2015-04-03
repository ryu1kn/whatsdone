
var React = require('react');

var converter = new Showdown.converter();

var DoneItem = React.createClass({

  formatTime: function (isoTimeString) {
    var date = new Date(isoTimeString),
        hour = date.getHours(),
        ampm = hour < 12 ? 'am' : 'pm',
        mins = date.getMinutes();

    if (hour > 12) {
      hour -= 12;
    }
    if (mins < 10) {
      mins = '0' + mins;
    }

    return `${hour}:${mins} ${ampm}`;
  },

  render: function() {
    var rawMarkup = converter.makeHtml(this.props.children.toString());
    return (
      <div className="doneitem">
        <div className="doneitem__done-thing"
             dangerouslySetInnerHTML={{__html: rawMarkup}} />
        <p className="doneitem__time">
          {this.formatTime(this.props.date)}
        </p>
      </div>
    );
  }
});

module.exports = DoneItem;
