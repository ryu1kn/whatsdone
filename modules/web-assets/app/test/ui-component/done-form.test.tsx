/**
 * @jest-environment jsdom
 */
import {fireEvent, render, screen} from '@testing-library/react';
import {DoneForm} from '../../src/done/components/form/component';

test('submits trimmed DONE text and clears the textarea', () => {
  const onSubmit = jest.fn();
  render(<DoneForm onSubmit={onSubmit} />);
  const textarea = screen.getByLabelText('Done Thing');
  fireEvent.change(textarea, {target: {value: '  Finished the upgrade  '}});
  fireEvent.click(screen.getByRole('button', {name: 'Done!'}));
  expect(onSubmit).toHaveBeenCalledWith('Finished the upgrade');
  expect((textarea as HTMLTextAreaElement).value).toBe('');
});

test('does not submit an empty DONE', () => {
  const onSubmit = jest.fn();
  render(<DoneForm onSubmit={onSubmit} />);
  fireEvent.change(screen.getByLabelText('Done Thing'), {target: {value: '  '}});
  fireEvent.click(screen.getByRole('button', {name: 'Done!'}));
  expect(onSubmit).not.toHaveBeenCalled();
});
