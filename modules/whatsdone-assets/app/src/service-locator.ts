import ServiceFactory from './service-factory';
import ConfigProvider from './config-provider';

export type SmartFetch = (uri: string, options?: RequestInit) => Promise<{status: number, body: any}>;

class ServiceLocator {
  private _serviceFactory: any;
  private _overrideServiceFactory: any;
  private _cache: {[k: string]: any};

  load(serviceFactory: ServiceFactory, overrideServiceFactory?: ServiceFactory) {
    this._serviceFactory = serviceFactory;
    this._overrideServiceFactory = overrideServiceFactory || Object.create(null);
    this._cache = Object.create(null);
  }

  get authTokenProvider() {
    return this._get('authTokenProvider');
  }

  get cognitoUserInitialiser() {
    return this._get('cognitoUserInitialiser');
  }

  get configProvider(): ConfigProvider {
    return this._get('configProvider');
  }

  get cookieStorage() {
    return this._get('cookieStorage');
  }

  get fetch(): typeof fetch {
    return this._get('fetch');
  }

  get smartFetch(): SmartFetch {
    return this._get('smartFetch');
  }

  get whatsdoneApiClient() {
    return this._get('whatsdoneApiClient');
  }

  _get(serviceName: string) {
    const cachedInstance = this._getCachedInstance(serviceName);
    if (cachedInstance) return cachedInstance;

    const instance = this._getFromServiceFactory(serviceName);
    this._cacheInstance(serviceName, instance);
    return instance;
  }

  _getCachedInstance(serviceName: string) {
    return this._cache[this._getCacheName(serviceName)];
  }

  _getFromServiceFactory(serviceName: string) {
    const methodName = this._getFactoryName(serviceName);

    return this._overrideServiceFactory[methodName] ?
      this._overrideServiceFactory[methodName]() :
      this._serviceFactory[methodName]();
  }

  // fooBar -> getFooBar
  _getFactoryName(name: string) {
    // @ts-ignore: name is not an empty string; hence name[0] cannot be undefined
    return ['create', name[0].toUpperCase(), name.substring(1)].join('');
  }

  _cacheInstance(serviceName: string, serviceInstance: any) {
    const cacheName = this._getCacheName(serviceName);
    this._cache[cacheName] = serviceInstance;
  }

  _getCacheName(name: string) {
    return `_${name}`;
  }

}

export default new ServiceLocator();
