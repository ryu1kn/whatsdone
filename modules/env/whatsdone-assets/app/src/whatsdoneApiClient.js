
import fetchFromWhatsdone from './FetchFromWhatsdone';

class WhatsdoneApiClient {

  login(loginDetails) {
    return fetchFromWhatsdone('/signin', this._buildPostOption(loginDetails));
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
    return fetchFromWhatsdone('/dones');
  }

  postDone(doneItem) {
    return fetchFromWhatsdone('/dones', this._buildPostOption(doneItem));
  }

  deleteDone(doneId) {
    return fetchFromWhatsdone(`/dones/${doneId}`, {method: 'DELETE'});
  }

}

module.exports = new WhatsdoneApiClient();
