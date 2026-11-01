import ServiceLocator from './service-locator';
import ConfigProvider from './config-provider';
import type {NextKey, RawDoneItem} from './done/reducer';
import type {SmartFetch, SmartFetchResponse} from './smart-fetch';
import AuthTokenProvider from './auth-token-provider';

const DEFAULT_OPTIONS = {
  mode: 'cors',
  credentials: 'include'
};

export type PostDoneItem = {
  doneThing: string
  date: string
}

export type PostDoneResponse = SmartFetchResponse & {body: RawDoneItem}

export type GetDonesResponse = SmartFetchResponse & {
  body: {
    items: RawDoneItem[],
    nextKey: NextKey
  }
}

class WhatsdoneApiClient {
  private readonly _smartFetch: SmartFetch;
  private readonly _configProvider: ConfigProvider;
  private readonly _authTokenProvider: AuthTokenProvider;

  constructor() {
    this._smartFetch = ServiceLocator.smartFetch;
    this._configProvider = ServiceLocator.configProvider;
    this._authTokenProvider = ServiceLocator.authTokenProvider;
  }

  getDones(nextKey?: NextKey): Promise<GetDonesResponse> {
    const qs = nextKey ? `?nextKey=${encodeURIComponent(nextKey)}` : '';
    return this._relayFetch(`/dones${qs}`);
  }

  postDone(doneItem: PostDoneItem): Promise<PostDoneResponse> {
    return this._relayFetch('/dones', withFormBody({
      method: 'POST',
      body: doneItem
    }));
  }

  deleteDone(doneId: string) {
    return this._relayFetch(`/dones/${doneId}`, {method: 'DELETE'});
  }

  updateDone(doneId: string, doneThing: string) {
    return this._relayFetch(`/dones/${doneId}`, withFormBody({
      method: 'PUT',
      body: {doneThing}
    }));
  }

  private _relayFetch(path: string, options?: RequestInit): Promise<SmartFetchResponse> {
    return Promise.all([this._getApiOrigin(), this._authTokenProvider.getIdToken()])
      .then(([apiOrigin, idToken]) => {
        const uri = new URL(path, apiOrigin).href;
        const finalHeaders = Object.assign({}, options?.headers, {Authorization: idToken});
        const finalOptions = Object.assign({}, DEFAULT_OPTIONS, options, {headers: finalHeaders});
        return this._smartFetch(uri, finalOptions);
      });
  }

  private _getApiOrigin() {
    return this._configProvider.getConfig()
      .then(appConfig => appConfig.API_ORIGIN);
  }
}

const withFormBody: (opts: Omit<RequestInit, 'body'> & {body: any}) => RequestInit =
  (options) => ({
    ...options,
    headers: {'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'},
    body: new URLSearchParams(options.body).toString()
  })

export default WhatsdoneApiClient;
