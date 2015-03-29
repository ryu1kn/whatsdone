
var React = require('react');

var DoneForm = React.createClass({
  handleSubmit: function(e) {
    e.preventDefault();
    var doneThing = this.refs.doneThing.getDOMNode().value.trim();
    if (!doneThing) {
      return;
    }
    this.props.onDoneItemSubmit({doneThing});
    this.refs.doneThing.getDOMNode().value = '';
  },
  render: function() {
    return (
      <form className="doneform" onSubmit={this.handleSubmit}>
        <input type="text" placeholder="What have you done today?" ref="doneThing" />
        <button type="submit">Done!</button>
      </form>
    );
  }
});

module.exports = DoneForm;
