import ServiceLocator from './service-locator';

class ConfigProvider {
  private readonly _smartFetch: any;
  private _APP_CONFIG: any;

  constructor() {
    this._smartFetch = ServiceLocator.smartFetch;
  }

  getConfig() {
    return this._APP_CONFIG ? Promise.resolve(this._APP_CONFIG) : this._fetchAppConfig();
  }

  _fetchAppConfig() {
    return this._smartFetch('/appConfig.json')
      .then(response => {
        this._APP_CONFIG = response.body;
        return this._APP_CONFIG;
      });
  }

}

export default ConfigProvider;
