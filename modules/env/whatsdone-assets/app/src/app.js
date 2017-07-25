
import React from 'react';
import ReactDOM from 'react-dom';
import {BrowserRouter as Router, Route, Switch} from 'react-router-dom';
import {Provider} from 'react-redux';
import {createStore, combineReducers} from 'redux';

import ServiceLocator from './service-locator';
import ServiceFactory from './service-factory';
ServiceLocator.load(new ServiceFactory());

import {requestGetDones, successGetDones, failedGetDones} from './done/actions';
import SigninPage from './signin/components/page';
import DonePage from './done/components/page';

const rootReducer = combineReducers({
  dones: require('./done/reducer'),
  loginStatus: require('./signin/reducer')
});
const store = createStore(rootReducer);

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
