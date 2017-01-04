
class ServiceLocator {

  load(serviceFactory) {
    this._serviceFactory = serviceFactory;
  }

  get authBasedRedirectMiddleware() {
    return this._get('authBasedRedirectMiddleware');
  }

  get schemaBasedRedirectMiddleware() {
    return this._get('schemaBasedRedirectMiddleware');
  }

  get getRootPageRequestHandler() {
    return this._get('getRootPageRequestHandler');
  }

  get getDonesRequestHandler() {
    return this._get('getDonesRequestHandler');
  }

  get getSigninRequestHandler() {
    return this._get('getSigninRequestHandler');
  }

  get postSigninRequestHandler() {
    return this._get('postSigninRequestHandler');
  }

  get signoutRequestHandler() {
    return this._get('signoutRequestHandler');
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
