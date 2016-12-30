
const ServiceLocator = require('../ServiceLocator');
const userDynamoTableClient = ServiceLocator.userDynamoTableClient;
const sha1 = require('sha1');

class Users {

  findUser(loginInfo) {
    return userDynamoTableClient.getByQuery({
      email: loginInfo.email,
      password: sha1(loginInfo.password)
    });
  }

  /**
   * @param {string} id
   * @return {q}
   */
  getById(id) {
    return userDynamoTableClient.getById(id);
  }

  /**
   * @param {Array<string>} ids
   * @return {q}
   */
  getByIds(ids) {
    return userDynamoTableClient.getByIds(ids);
  }

}

module.exports = new Users();
