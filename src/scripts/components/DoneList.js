
var React = require('react');

var DoneItem = require('./DoneItem');

var DoneList = React.createClass({
  render: function() {
    var doneItemNodes = this.props.data.map(function(doneItem, index) {
      return (
        <DoneItem date={doneItem.date} key={index}>
          {doneItem.doneThing}
        </DoneItem>
      );
    });
    return (
      <div className="donelist">
        <h2 className="donelist__title">{this.props.title}</h2>
        {doneItemNodes}
      </div>
    );
  }
});

module.exports = DoneList;
