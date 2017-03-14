
'use strict';

const _ = require('lodash');
const ServiceLocator = require('../ServiceLocator');

class PostSigninRequestProcessor {

  constructor() {
    this._userRepository = ServiceLocator.userRepository;
    this._htmlPageGenerator = ServiceLocator.htmlPageGenerator;
  }

  process(request) {
    const params = _.pick(request.body, ['email', 'password']);
    return this._userRepository.findUser(params).then(user => {
      if (user) {
        this._updateSessionInfo(request.session, user.id);
        return {
          statusCode: '303',
          headers: {Location: '/'}
        };
      }
      return {
        statusCode: '401',
        headers: {
          'Content-Type': 'text/html'
        },
        body: this._htmlPageGenerator.generate('signin')
      };
    });
  }

  _updateSessionInfo(session, userId) {
    Object.assign(session, {
      isAuthorized: true,
      userId: userId
    });
  }

}

module.exports = PostSigninRequestProcessor;
