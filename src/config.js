
var configs = {};

module.exports = {

  get: (key) => configs[key],

  set: (key, value) => {
    configs[key] = value;
  }

};
