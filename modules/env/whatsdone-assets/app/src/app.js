
import React from 'react';
import ReactDOM from 'react-dom';
import {BrowserRouter as Router, Route, Switch} from 'react-router-dom';
import {Provider} from 'react-redux';
import {createStore} from 'redux';

import reduce from './reducer';
import SigninPage from './pages/signin/index';
import DonePage from './pages/done/index';

const store = createStore(reduce);

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
