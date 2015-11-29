'use strict';

let Collection = new (require('./Collection'))('dones');
let Done = require('./Done');

class Dones {

  /**
   * @return {Q}
   */
  read() {
    return Collection.getAll();
  }

  /**
   * @param {{doneThing: string, date: string, userId: string}} newData
   * @return {Q}
   */
  write(newData) {
    return Collection.put(newData).then(id => Collection.getById(id));
  }

  /**
   * @param {string} id
   * @param {string} currentUserId
   */
  remove(id, currentUserId) {
    return Collection.getById(id)
      .then(found => {
        if (found === null) {
          throw new Error('[NotFound]: Done item not found');
        }
        if (found.userId !== currentUserId) {
          throw new Error('[AccessDeined]: You don\'t have the permission to delete this item.');
        }
        return Collection.delete(id);
      });
  }

  /**
   * @param {string} id
   * @param {string} currentUserId
   * @param {{doneThing: string, date: string}} newData
   */
  update(id, currentUserId, newData) {
    return Collection.getById(id)
      .then(found => {
        if (found === null) {
          throw new Error('[NotFound]: Done item not found');
        }
        if (found.userId !== currentUserId) {
          throw new Error('[AccessDeined]: You don\'t have the permission to modify this item.');
        }
        return Collection.update(id, Done.getModifiable(newData))
          .then(item => new Done(item._id, item.userId, item.doneThing, item.date));
      });
  }

}

module.exports = new Dones();