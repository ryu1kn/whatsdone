
const commonConfig = require('./common');

module.exports = Object.assign({},
  commonConfig,
  {
    customDeletionPolicy: 'Retain'
  }
);
