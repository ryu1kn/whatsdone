import ServiceLocator from './service-locator';

export interface AppConfig {
  CLIENT_ID: string
  USER_POOL_ID: string
  REGION: string
  IDENTITY_POOL_ID: string
  API_ORIGIN: string
}

class ConfigProvider {
  private readonly _smartFetch: any;
  private _APP_CONFIG: AppConfig;

  constructor() {
    this._smartFetch = ServiceLocator.smartFetch;
  }

  getConfig(): Promise<AppConfig> {
    return this._APP_CONFIG ? Promise.resolve(this._APP_CONFIG) : this._fetchAppConfig();
  }

  _fetchAppConfig() {
    return this._smartFetch('/appConfig.json')
      .then((response: {body: AppConfig}) => {
        this._APP_CONFIG = response.body;
        return this._APP_CONFIG;
      });
  }

}

export default ConfigProvider;
