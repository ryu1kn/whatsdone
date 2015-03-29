
var fs = require('fs');
var q = require('q');

module.exports = {

  DataFileName: 'dones.json',

  /**
   * @return {Q}
   */
  read: () =>
    q.nfcall(fs.readFile, this.DataFileName)
      .catch(() => '[]'),

  write: (newData) => {
    var fileName = this.DataFileName;
    return q.nfcall(fs.readFile, fileName)
      .catch(() => 'null')
      .then((storedData) => {
        var dones = (JSON.parse(storedData) || []).concat(newData);
        return q.nfcall(fs.writeFile, fileName, JSON.stringify(dones, null, 4))
          .then(() => dones);
      });
  }

};
