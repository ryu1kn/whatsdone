
import React from 'react';
import ReactDOM from 'react-dom';
import {BrowserRouter as Router, Route, Switch} from 'react-router-dom';
import {Provider} from 'react-redux';
import {createStore} from 'redux';

import ServiceLocator from './ServiceLocator';
import ServiceFactory from './ServiceFactory';
ServiceLocator.load(new ServiceFactory());

import {requestGetDones, successGetDones, failedGetDones} from './done/Actions';
import reduce from './reducer';
import SigninPage from './signin/index';
import DonePage from './done/index';

const store = createStore(reduce);

store.dispatch(requestGetDones());
ServiceLocator.whatsdoneApiClient.getDones().then(response => {
  store.dispatch(successGetDones(response));
}).catch(e => {
  store.dispatch(failedGetDones(e));
});

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
