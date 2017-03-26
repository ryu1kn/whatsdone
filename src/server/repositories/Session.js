
'use strict';

const ServiceLocator = require('../ServiceLocator');

class SessionRepository {

  constructor() {
    this._sessionDynamoTableClient = ServiceLocator.sessionDynamoTableClient;
  }

  write(newData) {
    return this._sessionDynamoTableClient.put(newData);
  }

  getById(sessionId) {
    return this._sessionDynamoTableClient.getById(sessionId);
  }

}

module.exports = SessionRepository;
