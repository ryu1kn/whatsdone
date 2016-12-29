
class ServiceLocator {

  load(serviceMap) {
    this._serviceMap = serviceMap;
  }

  _get(serviceName) {
    return this._serviceMap[serviceName];
  }

  get userRepository() {
    return this._get('userRepository');
  }

  get doneRepository() {
    return this._get('doneRepository');
  }

}

module.exports = new ServiceLocator();
