
class ServiceLocator {

  load(Context) {
    this._Context = Context;
  }

  get userRepository() {
    return this._get('userRepository');
  }

  get doneRepository() {
    return this._get('doneRepository');
  }

  get dynamoDB() {
    return this._get('dynamoDB');
  }

  get dynamoDBDocumentClient() {
    return this._get('dynamoDBDocumentClient');
  }

  _get(serviceName) {
    const methodName = this._getGetterName(serviceName);
    return this._Context[methodName]();
  }

  // fooBar -> getFooBar
  _getGetterName(name) {
    return ['get', name[0].toUpperCase(), name.substring(1)].join('');
  }

}

module.exports = new ServiceLocator();
