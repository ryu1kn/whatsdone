import ServiceLocator from './service-locator';
import querystring from 'querystring';
import url from 'url';
import ConfigProvider from './config-provider';
import {NextKey, RawDoneItem} from './done/reducer';
import {SmartFetch, SmartFetchResponse} from './smart-fetch';
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

  _buildPostOption(data: PostDoneItem) {
    return {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
      },
      body: querystring.stringify(data)
    };
  }

  getDones(nextKey?: NextKey): Promise<GetDonesResponse> {
    const qs = nextKey ? `?${querystring.stringify({nextKey})}` : '';
    return this._relayFetch(`/dones${qs}`);
  }

  postDone(doneItem: PostDoneItem): Promise<PostDoneResponse> {
    return this._relayFetch('/dones', this._buildPostOption(doneItem));
  }

  deleteDone(doneId: string) {
    return this._relayFetch(`/dones/${doneId}`, {method: 'DELETE'});
  }

  _relayFetch(path: string, options?: RequestInit): Promise<SmartFetchResponse> {
    return Promise.all([this._getApiOrigin(), this._authTokenProvider.getIdToken()])
      .then(([apiOrigin, idToken]) => {
        const uri = url.resolve(apiOrigin, path);
        const finalHeaders = Object.assign({}, options?.headers, {Authorization: idToken});
        const finalOptions = Object.assign({}, DEFAULT_OPTIONS, options, {headers: finalHeaders});
        return this._smartFetch(uri, finalOptions);
      });
  }

  _getApiOrigin() {
    return this._configProvider.getConfig()
      .then(appConfig => appConfig.API_ORIGIN);
  }

}

export default WhatsdoneApiClient;
