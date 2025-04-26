/**
 * @jest-environment jsdom
 */
import {render, waitFor} from '@testing-library/react';
import React from 'react';
import 'regenerator-runtime/runtime';
import {Provider} from 'react-redux';
import DonePage from '../../src/done/components/page';
import reducer from '../../src/reducer';
import {createStore} from 'redux';
import WhatsdoneApiClient from '../../src/whatsdone-api-client';
import ServiceFactory from '../../src/service-factory';
import ServiceLocator from '../../src/service-locator';

test('Show easy to read time of the user region (This test passes in Melbourne)', async () => {
  ServiceLocator.load({
    createWhatsdoneApiClient: () => ({getDones: (_nextKey?) => Promise.resolve({
        status: 200,
        body: {
          items: [{
            date: '2021-06-25T12:06:00Z',
            doneThing: 'DONE SOMETHING',
            id: 'DONE_ID',
            userId: 'USER_ID',
            username: 'USERNAME'
          }]
        }
    })} as WhatsdoneApiClient)
  } as ServiceFactory);

  const store = createStore(reducer);
  store.dispatch({type: 'API_READY'})

  const { getByText } = render(
    <Provider store={store}>
      <DonePage/>
    </Provider>
  )

  await waitFor(() => getByText('10:06 pm'))
});

test('Shows topic tags when topic tagging is enabled', async () => {
  ServiceLocator.load({
    createWhatsdoneApiClient: () => ({getDones: (_nextKey?) => Promise.resolve({
        status: 200,
        body: {
          items: [{
            date: '2021-06-25T12:06:00Z',
            doneThing: 'DONE SOMETHING',
            id: 'DONE_ID',
            userId: 'USER_ID',
            username: 'USERNAME',
            topics: ['Development', 'Operations']
          }]
        }
    })} as WhatsdoneApiClient)
  } as ServiceFactory);

  const store = createStore(reducer);
  store.dispatch({type: 'API_READY'});
  store.dispatch({type: 'INIT_FEATURES', features: ['topicTagging']});

  const { getByText } = render(
    <Provider store={store}>
      <DonePage/>
    </Provider>
  );

  await waitFor(() => {
    getByText('Development');
    getByText('Operations');
  });
});
