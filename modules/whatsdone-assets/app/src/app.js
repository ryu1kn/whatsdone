
import 'bootstrap/dist/css/bootstrap.css';
import 'bootstrap/dist/js/bootstrap.min';

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

const store = createStore(
  reducer,
  window.__REDUX_DEVTOOLS_EXTENSION__ && window.__REDUX_DEVTOOLS_EXTENSION__()
);
ServiceLocator.cognitoUserInitialiser.initialise()
  .then(() => store.dispatch({type: 'API_READY'}));

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
