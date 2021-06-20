import React from 'react';

interface DoneFormProps {
  onSubmit: (doneThing: string) => void
}

export class DoneForm extends React.Component<DoneFormProps> {

  handleSubmit(e) {
    e.preventDefault();
    const doneThingElement = this.refs.doneThing as HTMLInputElement;
    const doneThing = doneThingElement.value.trim();
    if (!doneThing) return;

    this.props.onSubmit(doneThing);
    doneThingElement.value = '';
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
