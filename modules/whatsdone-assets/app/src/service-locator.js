
class ServiceLocator {

  load(serviceFactory) {
    this._serviceFactory = serviceFactory;
    this._cache = Object.create(null);
  }

  get authenticator() {
    return this._get('authenticator');
  }

  get authTokenProvider() {
    return this._get('authTokenProvider');
  }

  get configProvider() {
    return this._get('configProvider');
  }

  get cookieStorage() {
    return this._get('cookieStorage');
  }

  get fetch() {
    return this._get('fetch');
  }

  get smartFetch() {
    return this._get('smartFetch');
  }

  get whatsdoneApiClient() {
    return this._get('whatsdoneApiClient');
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

export default new ServiceLocator();
