
import '../../stylesheets/base.less';
import '../../stylesheets/signin.less';

import React from 'react';
import ReactDOM from 'react-dom';
import {BrowserRouter as Router, Route, Switch} from 'react-router-dom';

import SigninPage from './components/Signin.react';
import DoneBox from './components/DoneBox.react';

ReactDOM.render(
  (
    <Router>
      <Switch>
        <Route exact path="/" component={DoneBox} />
        <Route path="/signin" component={SigninPage} />
      </Switch>
    </Router>
  ),
  document.getElementById('content')
);
