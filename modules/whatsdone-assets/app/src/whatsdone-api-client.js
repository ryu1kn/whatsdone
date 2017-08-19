
import ServiceLocator from './service-locator';
import url from 'url';

const DEFAULT_OPTIONS = {
  mode: 'cors',
  credentials: 'include'
};

class WhatsdoneApiClient {

  constructor() {
    this._smartFetch = ServiceLocator.smartFetch;
  }

  login(loginDetails) {
    return this._relayFetch('/signin', this._buildPostOption(loginDetails));
  }

  _buildPostOption(data) {
    return {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
      },
      body: this._composeSearchParams(data)
    };
  }

  _composeSearchParams(data) {
    return Object.keys(data)
      .map(key => `${encodeURIComponent(key)}=${encodeURIComponent(data[key])}`)
      .join('&');
  }

  getDones(nextPageKey) {
    const qs = nextPageKey ? `?nextKey=${encodeURIComponent(nextPageKey)}` : '';
    return this._relayFetch(`/dones${qs}`);
  }

  postDone(doneItem) {
    return this._relayFetch('/dones', this._buildPostOption(doneItem));
  }

  deleteDone(doneId) {
    return this._relayFetch(`/dones/${doneId}`, {method: 'DELETE'});
  }

  _relayFetch(path, options) {
    return this._getApiOrigin().then(apiOrigin => {
      const uri = url.resolve(apiOrigin, path);
      const finalOptions = Object.assign({}, DEFAULT_OPTIONS, options);
      return this._smartFetch(uri, finalOptions);
    });
  }

  // HACK: Temporally placing the retrieval of application config here
  _getApiOrigin() {
    if (this._API_ORIGIN) return Promise.resolve(this._API_ORIGIN);
    return this._smartFetch('/appConfig.json')
      .then(response => {
        this._API_ORIGIN = response.body.API_ORIGIN;
        return this._API_ORIGIN;
      });
  }

}

export default WhatsdoneApiClient;
