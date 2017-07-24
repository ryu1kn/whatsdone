
import React from 'react';

class DoneForm extends React.Component {

  handleSubmit(e) {
    e.preventDefault();
    const doneThing = this.refs.doneThing.value.trim();
    if (!doneThing) return;

    this.props.onSubmit(doneThing);
    this.refs.doneThing.value = '';
  }

  render() {
    return (
      <form className="form-inline doneform" onSubmit={this.handleSubmit.bind(this)}>
        <div className="form-group">
          <label className="sr-only" htmlFor="doneInput">Done Thing</label>
          <input type="text" className="form-control" id="doneInput"
                 placeholder="What have you done today?" ref="doneThing" />
        </div>
        <button type="submit" className="btn btn-default">Done!</button>
      </form>
    );
  }

}

module.exports = DoneForm;
