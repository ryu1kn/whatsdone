'use strict';

let Collection = new (require('./Collection'))('users');
let sha1 = require('sha1');

class Users {

  findUser(loginInfo) {
    return Collection.getByQuery({
      email: loginInfo.email,
      password: sha1(loginInfo.password)
    });
  }

  /**
   * @param {string} id
   * @return {q}
   */
  getById(id) {
    return Collection.getById(id);
  }

  /**
   * @param {Array<string>} ids
   * @return {q}
   */
  getByIds(ids) {
    return Collection.getByIds(ids);
  }

}

module.exports = new Users();
