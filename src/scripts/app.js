
var React = require('react');

var DoneBox = require('./components/DoneBox');

React.render(
  <DoneBox url="dones.json" pollInterval={2000} />,
  document.getElementById('content')
);
