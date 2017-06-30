
require('../../stylesheets/base.less');
require('../../stylesheets/signin.less');

var React = require('react');
var ReactDOM = require('react-dom');

var DoneBox = require('./components/DoneBox.react');

ReactDOM.render(
  <DoneBox />,
  document.getElementById('content')
);
