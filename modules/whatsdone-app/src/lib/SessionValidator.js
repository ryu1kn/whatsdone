
'use strict';

const ServiceLocator = require('./ServiceLocator');

const DAY_IN_MSEC = 24 * 60 * 60 * 1000;
const SESSION_TTL = 7 * DAY_IN_MSEC;

class SessionValidator {

  constructor() {
    this._dateProvider = ServiceLocator.dateProvider;
  }

  validate(session) {
    if (!session || !session.createdAt) return false;
    return this._isValid(new Date(session.createdAt));
  }

  _isValid(createdDate) {
    const deltaInMSec = this._dateProvider.getCurrentDate() - createdDate;
    const isExpired = deltaInMSec > SESSION_TTL;
    const isFutureDate = deltaInMSec < 0;
    return !(isFutureDate || isExpired);
  }

}

module.exports = SessionValidator;
