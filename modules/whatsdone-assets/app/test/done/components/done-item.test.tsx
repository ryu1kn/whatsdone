/**
 * @jest-environment jsdom
 */
import {DoneItem} from '../../../src/done/components/done-item/component';
import {render, screen, waitFor} from '@testing-library/react';
import React from 'react';
import 'regenerator-runtime/runtime';

test('Show easy to read time of the user region (This test passes in Melbourne)', async () => {
  render(
    <DoneItem doneId={'DONE_ID'} date={new Date('2021-06-25T12:06:00Z')}
      username={'USERNAME'} key={0} deleteDone={() => {}}>
      {'DONE SOMETHING'}
    </DoneItem>
  )

  await waitFor(() => screen.getByText('10:06 pm'))
});
