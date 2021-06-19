import ServiceLocator from './service-locator';
import querystring from 'querystring';
import url from 'url';

const DEFAULT_OPTIONS = {
  mode: 'cors',
  credentials: 'include'
};

class WhatsdoneApiClient {
  private readonly _smartFetch: any;
  private readonly _configProvider: any;
  private readonly _authTokenProvider: any;

  constructor() {
    this._smartFetch = ServiceLocator.smartFetch;
    this._configProvider = ServiceLocator.configProvider;
    this._authTokenProvider = ServiceLocator.authTokenProvider;
  }

  _buildPostOption(data) {
    return {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
      },
      body: querystring.stringify(data)
    };
  }

  getDones(nextKey?) {
    const qs = nextKey ? `?${querystring.stringify({nextKey})}` : '';
    return this._relayFetch(`/dones${qs}`);
  }

  postDone(doneItem) {
    return this._relayFetch('/dones', this._buildPostOption(doneItem));
  }

  deleteDone(doneId) {
    return this._relayFetch(`/dones/${doneId}`, {method: 'DELETE'});
  }

  _relayFetch(path, options?) {
    return Promise.all([this._getApiOrigin(), this._authTokenProvider.getIdToken()])
      .then(([apiOrigin, idToken]) => {
        const uri = url.resolve(apiOrigin, path);
        const headers = (options && options.headers) || {};
        const finalHeaders = Object.assign({}, headers, {Authorization: idToken});
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
