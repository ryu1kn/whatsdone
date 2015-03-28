/**
 * This file provided by Facebook is for non-commercial testing and evaluation purposes only.
 * Facebook reserves all rights not expressly granted.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * FACEBOOK BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

var fs = require('fs');
var path = require('path');
var Q = require('q');
var express = require('express');
var bodyParser = require('body-parser');
var app = express();

app.use('/', express.static(path.join(__dirname, 'public')));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({extended: true}));

app.get('/dones.json', function(req, res) {
  Q.nfcall(fs.readFile, 'dones.json')
    .catch(function () {
      return '[]';
    })
    .then(function (data) {
      res.setHeader('Content-Type', 'application/json');
      res.send(data);
    });
});

app.post('/dones.json', function(req, res) {
  Q.nfcall(fs.readFile, 'dones.json')
    .catch(function () {
      return 'null';
    })
    .then(function (data) {
      var dones = (JSON.parse(data) || []).concat(req.body);
      return Q.nfcall(fs.writeFile, 'dones.json', JSON.stringify(dones, null, 4))
        .then(function () {
          res.setHeader('Content-Type', 'application/json');
          res.setHeader('Cache-Control', 'no-cache');
          res.send(JSON.stringify(dones));
        });
    })
    .catch(function (reason) {
      console.error(reason);
      res.status(500);
      res.send('500: Internal Server Error');
    })
    .done();
});

app.listen(3000);

console.log('Server started: http://localhost:3000/');
