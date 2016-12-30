
const ServiceLocator = require('../ServiceLocator');
const doneDynamoTableClient = ServiceLocator.doneDynamoTableClient;
const Done = require('../Done');

class Dones {

  /**
   * @return {Q}
   */
  read() {
    return doneDynamoTableClient.getAll();
  }

  /**
   * @param {{doneThing: string, date: string, userId: string}} newData
   * @return {Q}
   */
  write(newData) {
    return doneDynamoTableClient.put(newData).then(id => doneDynamoTableClient.getById(id));
  }

  /**
   * @param {string} id
   * @param {string} currentUserId
   */
  remove(id, currentUserId) {
    return doneDynamoTableClient.getById(id)
      .then(found => {
        if (found === null) {
          throw new Error('[NotFound]: Done item not found');
        }
        if (found.userId !== currentUserId) {
          throw new Error('[AccessDeined]: You don\'t have the permission to delete this item.');
        }
        return doneDynamoTableClient.delete(id);
      });
  }

  /**
   * @param {string} id
   * @param {string} currentUserId
   * @param {{doneThing: string, date: string}} newData
   */
  update(id, currentUserId, newData) {
    return doneDynamoTableClient.getById(id)
      .then(found => {
        if (found === null) {
          throw new Error('[NotFound]: Done item not found');
        }
        if (found.userId !== currentUserId) {
          throw new Error('[AccessDeined]: You don\'t have the permission to modify this item.');
        }
        return doneDynamoTableClient.update(id, Done.getModifiable(newData))
          .then(item => new Done(item.id, item.userId, item.doneThing, item.date));
      });
  }

}

module.exports = Dones;
