
'use strict';

class SessionValidator {

  validate(session) {
    return !!(session && session.isAuthorized);
  }

}

module.exports = SessionValidator;
