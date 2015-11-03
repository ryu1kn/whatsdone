
var Collection = new (require('./Collection'))('dones');
var Done = require('./Done');

function deleteDone(id, currentUserId) {
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

function updateDone(id, currentUserId, newData) {
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

module.exports = {

  /**
   * @return {Q}
   */
  read: () => Collection.getAll(),

  /**
   * @param {{doneThing: string, date: string, userId: string}} newData
   * @return {Q}
   */
  write: (newData) =>
      Collection.put(newData).then(id => Collection.getById(id)),

  /**
   * @param {string} id
   * @param {string} currentUserId
   */
  remove: (id, currentUserId) => deleteDone(id, currentUserId),

  /**
   * @param {string} id
   * @param {string} currentUserId
   * @param {{doneThing: string, date: string}} newData
   */
  update: (id, currentUserId, newData) => updateDone(id, currentUserId, newData)

};
