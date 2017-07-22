
import ServiceLocator from './ServiceLocator';

class WhatsdoneApiClient {

  constructor() {
    this._fetchFromWhatsdone = ServiceLocator.fetchFromWhatsdone;
  }

  login(loginDetails) {
    return this._fetchFromWhatsdone('/signin', this._buildPostOption(loginDetails));
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

  getDones() {
    return this._fetchFromWhatsdone('/dones');
  }

  postDone(doneItem) {
    return this._fetchFromWhatsdone('/dones', this._buildPostOption(doneItem));
  }

  deleteDone(doneId) {
    return this._fetchFromWhatsdone(`/dones/${doneId}`, {method: 'DELETE'});
  }

}

module.exports = WhatsdoneApiClient;
