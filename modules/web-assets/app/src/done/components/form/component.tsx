import React from 'react';

interface DoneFormProps {
  onSubmit: (doneThing: string) => void
}

export class DoneForm extends React.Component<DoneFormProps> {

  private readonly doneThingRef = React.createRef<HTMLTextAreaElement>();

  private handleSubmit(e: any) {
    e.preventDefault();
    const doneThingElement = this.doneThingRef.current!;
    const doneThing = doneThingElement.value.trim();
    if (!doneThing) return;

    this.props.onSubmit(doneThing);
    doneThingElement.value = '';
  }

  render() {
    return (
      <form className="doneform" onSubmit={this.handleSubmit.bind(this)}>
        <div className="mb-2">
          <label className="visually-hidden" htmlFor="doneInput">Done Thing</label>
          <textarea className="form-control" id="doneInput"
            placeholder="What have you done today?" ref={this.doneThingRef} rows={6} />
        </div>
        <button type="submit" className="btn btn-outline-secondary doneform__button">Done!</button>
      </form>
    );
  }
}
