
class ServiceLocator {
  private _serviceFactory: any;
  private _cache: any;

  load(serviceFactory) {
    this._serviceFactory = serviceFactory;
    this._cache = {};
  }

  get config() {
    return this._get('config');
  }

  get accessLogger() {
    return this._get('accessLogger');
  }

  get cognitoIdentityServiceProvider() {
    return this._get('cognitoIdentityServiceProvider');
  }

  get cognitoUserFinder() {
    return this._get('cognitoUserFinder');
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

  get staticContentsProvider() {
    return this._get('staticContentsProvider');
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

  get createDoneCommand() {
    return this._get('createDoneCommand');
  }

  get deleteDoneCommand() {
    return this._get('deleteDoneCommand');
  }

  get getDonesCommand() {
    return this._get('getDonesCommand');
  }

  get updateDoneCommand() {
    return this._get('updateDoneCommand');
  }

  get requestProcessErrorProcessor() {
    return this._get('requestProcessErrorProcessor');
  }

  get lambdaRequestNormaliser() {
    return this._get('lambdaRequestNormaliser');
  }

  get lambdaResponseFormatter() {
    return this._get('lambdaResponseFormatter');
  }

  get logger() {
    return this._get('logger');
  }

  get userNameService() {
    return this._get('userNameService');
  }

  get userIdRepository() {
    return this._get('userIdRepository');
  }

  get doneRepository() {
    return this._get('doneRepository');
  }

  get dynamoDBDocumentClient() {
    return this._get('dynamoDBDocumentClient');
  }

  get doneQueryHelper() {
    return this._get('doneQueryHelper');
  }

  get doneDynamoTableClient() {
    return this._get('doneDynamoTableClient');
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

export = new ServiceLocator();
