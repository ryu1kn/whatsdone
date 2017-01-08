
class ServiceLocator {

  load(serviceFactory) {
    this._serviceFactory = serviceFactory;
  }

  get accessLogger() {
    return this._get('accessLogger');
  }

  get cookieParser() {
    return this._get('cookieParser');
  }

  get encodedUrlParser() {
    return this._get('encodedUrlParser');
  }

  get jsonRequestBodyParser() {
    return this._get('jsonRequestBodyParser');
  }

  get sessionManager() {
    return this._get('sessionManager');
  }

  get staticContentsProvider() {
    return this._get('staticContentsProvider');
  }

  get viewDirectoryPath() {
    return this._get('viewDirectoryPath');
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

  get postDonesRequestHandler() {
    return this._get('postDonesRequestHandler');
  }

  get deleteDoneRequestHandler() {
    return this._get('deleteDoneRequestHandler');
  }

  get updateDoneRequestHandler() {
    return this._get('updateDoneRequestHandler');
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

  get noMatchingRouteRequestHandler() {
    return this._get('noMatchingRouteRequestHandler');
  }

  get errorHandler() {
    return this._get('errorHandler');
  }

  get logger() {
    return this._get('logger');
  }

  get userRepository() {
    return this._get('userRepository');
  }

  get doneRepository() {
    return this._get('doneRepository');
  }

  get doneFormatter() {
    return this._get('doneFormatter');
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
