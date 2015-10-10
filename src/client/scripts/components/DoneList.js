
var _ = require('lodash');
var React = require('react');

var DoneItem = require('./DoneItem');

var DoneList = React.createClass({
  render: function() {
    var doneItemNodes = this.props.data.map(function(doneItem, index) {
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
});

module.exports = DoneList;
