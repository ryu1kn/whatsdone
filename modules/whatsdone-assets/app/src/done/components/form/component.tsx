import React from 'react';

interface DoneFormProps {
  onSubmit: (doneThing: string) => void
}

export class DoneForm extends React.Component<DoneFormProps> {

  private handleSubmit(e: any) {
    e.preventDefault();
    const doneThingElement = this.refs.doneThing as HTMLTextAreaElement;
    const doneThing = doneThingElement.value.trim();
    if (!doneThing) return;

    this.props.onSubmit(doneThing);
    doneThingElement.value = '';
  }

  render() {
    return (
      <form className="doneform" onSubmit={this.handleSubmit.bind(this)}>
        <div className="form-group">
          <label className="sr-only" htmlFor="doneInput">Done Thing</label>
          <textarea className="form-control" id="doneInput"
            placeholder="What have you done today?" ref="doneThing" rows={6} />
        </div>
        <button type="submit" className="btn btn-default doneform__button">Done!</button>
      </form>
    );
  }
}
