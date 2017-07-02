
const express = require('express');
const cors = require('cors');
const app = express();

var corsOptions = {
  origin: 'http://localhost:8080',
  optionsSuccessStatus: 200 // some legacy browsers (IE11, various SmartTVs) choke on 204
};

app.get('/dones', cors(corsOptions), (req, res) => {
  res.sendStatus(401);
});

app.post('/signin', cors(corsOptions), (req, res) => {
  res.sendStatus(200);
});

app.get('*', function (req, res) {
  res.sendStatus(404);
});

var PORT = process.env.PORT || 8081;

app.listen(PORT, () => {
  console.log(`Production Express server running at localhost: ${PORT}`);
});
