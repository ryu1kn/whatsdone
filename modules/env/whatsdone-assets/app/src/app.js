
import React from 'react';
import ReactDOM from 'react-dom';
import {BrowserRouter as Router, Route, Switch} from 'react-router-dom';
import {Provider} from 'react-redux';
import {createStore} from 'redux';

import ServiceLocator from './service-locator';
import ServiceFactory from './service-factory';
ServiceLocator.load(new ServiceFactory());

import reducer from './reducer';
import SigninPage from './signin/components/page';
import DonePage from './done/components/page';
import DoneAction from './done/action';

const store = createStore(reducer);
populateStore(store);

ReactDOM.render(
  (
    <Provider store={store}>
      <Router>
        <Switch>
          <Route exact path="/" component={DonePage} />
          <Route path="/signin" component={SigninPage} />
        </Switch>
      </Router>
    </Provider>
  ),
  document.getElementById('content')
);

function populateStore(store) {
  store.dispatch(DoneAction.getDones());
  ServiceLocator.whatsdoneApiClient.getDones()
    .then(response => {
      store.dispatch(DoneAction.markGetDonesSuccess(response));
    }).catch(e => {
      store.dispatch(DoneAction.markGetDonesFailed(e));
    });
}
