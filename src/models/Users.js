
var Collection = new (require('./Collection'))('users');
var sha1 = require('sha1');

module.exports = {

  findUser: loginInfo => Collection.getByQuery({
      email   : loginInfo.email,
      password: sha1(loginInfo.password)
    }),

  /**
   * @param {string} id
   * @return {q}
   */
  getById: id => Collection.getById(id),

  /**
   * @param {Array<string>} ids
   * @return {q}
   */
  getByIds: ids => Collection.getByIds(ids)

};
