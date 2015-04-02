
var React = require('react');

var DoneBox = require('./components/DoneBox');

React.render(
  <DoneBox url="dones.json" />,
  document.getElementById('content')
);
