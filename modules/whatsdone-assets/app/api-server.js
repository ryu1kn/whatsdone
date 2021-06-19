
const express = require('express');
const querystring = require('querystring');
const cors = require('cors');
const app = express();

const corsOptions = {
  origin: 'http://localhost:8080',
  credentials: true,
  methods: ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS'],
  optionsSuccessStatus: 200 // some legacy browsers (IE11, various SmartTVs) choke on 204
};

const dones = dummyDones();

app.options('/dones', cors(corsOptions))

app.get('/dones', cors(corsOptions), (req, res) => {
  res.status(200).json({
    items: dones
  });
});

app.post('/dones', cors(corsOptions), (req, res) => {
  promiseToGetBody(req)
    .then(bodyString => {
      const parsedBody = querystring.parse(bodyString);
      const newDone = Object.assign({}, parsedBody, {id: generateDummyId()});
      dones.push(newDone);
      res.status(202).json(newDone);
    })
    .catch(e => console.error(e.stack));
});

app.get('*', (req, res) => {
  res.sendStatus(404);
});

const PORT = process.env.PORT || 8081;

app.listen(PORT, () => {
  console.log(`Production Express server running at localhost: ${PORT}`);
});

function promiseToGetBody(req) {
  return new Promise((resolve, reject) => {
    const tmp = {string: ''};
    req.on('data', data => {
      tmp.string += data.toString();
    });
    req.on('end', () => resolve(tmp.string));
    req.on('error', e => reject(e));
  });
}

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

function generateDummyId() {
  return `DUMMY_ID_${new Date().getTime()}`;
}
