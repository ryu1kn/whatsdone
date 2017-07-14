
const express = require('express');
const cors = require('cors');
const app = express();

var corsOptions = {
  origin: 'http://localhost:8080',
  credentials: true,
  optionsSuccessStatus: 200 // some legacy browsers (IE11, various SmartTVs) choke on 204
};

app.get('/dones', cors(corsOptions), (req, res) => {
  res.status(200).json(dummyDones());
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

function dummyDones() {
  return [
    {
      doneThing: 'This is my first done!',
      username: 'ryuichi',
      date: '2016-03-27T12:42:32.625Z',
      userId: 'userid000000000000000001',
      id: 'XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX'
    },
    {
      doneThing: 'Awesome done thing',
      username: 'ryuichi',
      date: '2015-04-28T07:03:08.629Z',
      userId: 'userid000000000000000001',
      id: 'YYYYYYYY-YYYY-YYYY-YYYY-YYYYYYYYYYYY'
    }
  ];
}
