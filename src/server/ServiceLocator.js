
'use strict';

class ServiceLocator {

  load(serviceFactory) {
    this._serviceFactory = serviceFactory;
    this._cache = {};
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

  get faviconProvider() {
    return this._get('faviconProvider');
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

  get authBasedRedirector() {
    return this._get('authBasedRedirector');
  }

  get getRootPageRequestHandler() {
    return this._get('getRootPageRequestHandler');
  }

  get getDonesRequestHandler() {
    return this._get('getDonesRequestHandler');
  }

  get postDoneRequestHandler() {
    return this._get('postDoneRequestHandler');
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

  get createDoneCommand() {
    return this._get('createDoneCommand');
  }

  get deleteDoneCommand() {
    return this._get('deleteDoneCommand');
  }

  get getDonesCommand() {
    return this._get('getDonesCommand');
  }

  get loginCommand() {
    return this._get('loginCommand');
  }

  get updateDoneCommand() {
    return this._get('updateDoneCommand');
  }

  get requestProcessErrorProcessor() {
    return this._get('requestProcessErrorProcessor');
  }

  get expressRequestNormaliser() {
    return this._get('expressRequestNormaliser');
  }

  get expressResponseSenderFactory() {
    return this._get('expressResponseSenderFactory');
  }

  get logger() {
    return this._get('logger');
  }

  get pug() {
    return this._get('pug');
  }

  get cookieCodec() {
    return this._get('cookieCodec');
  }

  get sessionValidator() {
    return this._get('sessionValidator');
  }

  get htmlPageGenerator() {
    return this._get('htmlPageGenerator');
  }

  get userRepository() {
    return this._get('userRepository');
  }

  get doneRepository() {
    return this._get('doneRepository');
  }

  get sessionRepository() {
    return this._get('sessionRepository');
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

  get sessionDynamoTableClient() {
    return this._get('sessionDynamoTableClient');
  }

  get hashGenerator() {
    return this._get('hashGenerator');
  }

  get uuidGenerator() {
    return this._get('uuidGenerator');
  }

  get dateProvider() {
    return this._get('dateProvider');
  }

  _get(serviceName) {
    const cachedInstance = this._getCachedInstance(serviceName);
    if (cachedInstance) return cachedInstance;

    const instance = this._getFromServiceFactory(serviceName);
    this._cacheInstance(serviceName, instance);
    return instance;
  }

  _getCachedInstance(serviceName) {
    return this._cache[this._getCacheName(serviceName)];
  }

  _getFromServiceFactory(serviceName) {
    const methodName = this._getFactoryName(serviceName);
    return this._serviceFactory[methodName]();
  }

  // fooBar -> getFooBar
  _getFactoryName(name) {
    return ['create', name[0].toUpperCase(), name.substring(1)].join('');
  }

  _cacheInstance(serviceName, serviceInstance) {
    const cacheName = this._getCacheName(serviceName);
    this._cache[cacheName] = serviceInstance;
  }

  _getCacheName(name) {
    return `_${name}`;
  }

}

module.exports = new ServiceLocator();
