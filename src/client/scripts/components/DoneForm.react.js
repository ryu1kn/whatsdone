
var React = require('react');
var DoneAction = require('../actions/DoneAction');

var DoneForm = React.createClass({

  handleSubmit: function(e) {
    e.preventDefault();
    var doneThing = this.refs.doneThing.getDOMNode().value.trim();
    if (!doneThing) {
      return;
    }
    DoneAction.create(doneThing);
    this.refs.doneThing.getDOMNode().value = '';
  },

  render: function() {
    return (
      <form className="form-inline doneform" onSubmit={this.handleSubmit}>
        <div className="form-group">
          <label className="sr-only" htmlFor="doneInput">Done Thing</label>
          <input type="text" className="form-control" id="doneInput"
                 placeholder="What have you done today?" ref="doneThing" />
        </div>
        <button type="submit" className="btn btn-default">Done!</button>
      </form>
    );
  }
});

module.exports = DoneForm;
