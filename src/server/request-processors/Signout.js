
'use strict';

class SignoutRequestProcessor {

  process(request) {
    delete request.session.isAuthorized;
    return {
      statusCode: '303',
      headers: {
        Location: '/signin'
      }
    };
  }

}

module.exports = SignoutRequestProcessor;
