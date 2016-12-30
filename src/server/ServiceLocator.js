
class ServiceLocator {

  load(serviceFactory) {
    this._serviceFactory = serviceFactory;
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

  get doneDynamoTableClient() {
    return this._get('doneDynamoTableClient');
  }

  get userDynamoTableClient() {
    return this._get('userDynamoTableClient');
  }

  get hashGenerator() {
    return this._get('hashGenerator');
  }

  get uuidGenerator() {
    return this._get('uuidGenerator');
  }

  _get(serviceName) {
    const methodName = this._getGetterName(serviceName);
    return this._serviceFactory[methodName]();
  }

  // fooBar -> getFooBar
  _getGetterName(name) {
    return ['get', name[0].toUpperCase(), name.substring(1)].join('');
  }

}

module.exports = new ServiceLocator();
