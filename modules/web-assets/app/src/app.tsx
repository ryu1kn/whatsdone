import 'bootstrap/dist/css/bootstrap.css';
import 'bootstrap/dist/js/bootstrap.min';

import React from 'react';
import ReactDOM from 'react-dom';
import {BrowserRouter as Router, Route, Switch} from 'react-router-dom';
import {Provider} from 'react-redux';
import {createStore} from 'redux';

import ServiceLocator from './service-locator';
import ServiceFactory from './service-factory';
import reducer from './reducer';
import DonePage from './done/components/page';

ServiceLocator.load(new ServiceFactory());

const store = createStore(
  reducer,
  window.__REDUX_DEVTOOLS_EXTENSION__?.()
);

// Initialize features based on URL parameters
const feature = new URLSearchParams(window.location.search).get('feature');
store.dispatch({
  type: 'INIT_FEATURES',
  features: feature ? [feature] : []
});

ServiceLocator.cognitoUserInitialiser.initialise()
  .then(() => {
    window.history.replaceState({}, '', window.location.pathname + window.location.search);
  })
  .then(() => store.dispatch({type: 'API_READY'}))
  .catch((e: Error) => { console.error(e.stack); });

ReactDOM.render(
  (
    <Provider store={store}>
      <Router>
        <Switch>
          <Route exact path="/" component={DonePage} />
        </Switch>
      </Router>
    </Provider>
  ),
  document.getElementById('content')
);
