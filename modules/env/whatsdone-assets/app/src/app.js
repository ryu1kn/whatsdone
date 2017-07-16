
import React from 'react';
import ReactDOM from 'react-dom';
import {BrowserRouter as Router, Route, Switch} from 'react-router-dom';

import SigninPage from './pages/signin/index';
import DonePage from './pages/done/index';

ReactDOM.render(
  (
    <Router>
      <Switch>
        <Route exact path="/" component={DonePage} />
        <Route path="/signin" component={SigninPage} />
      </Switch>
    </Router>
  ),
  document.getElementById('content')
);
