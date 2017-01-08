
const ServiceLocator = require('../ServiceLocator');
const Done = require('../Done');

class DoneRepository {

  constructor() {
    this._doneDynamoTableClient = ServiceLocator.doneDynamoTableClient;
  }

  read() {
    return this._doneDynamoTableClient.getAll();
  }

  write(newData) {
    return this._doneDynamoTableClient.put(newData).then(id => this._doneDynamoTableClient.getById(id));
  }

  remove(id, currentUserId) {
    return this._doneDynamoTableClient.getById(id)
      .then(found => {
        if (found === null) {
          throw new Error('[NotFound]: Done item not found');
        }
        if (found.userId !== currentUserId) {
          throw new Error('[AccessDeined]: You don\'t have the permission to delete this item.');
        }
        return this._doneDynamoTableClient.delete(id);
      });
  }

  update(id, currentUserId, newData) {
    return this._doneDynamoTableClient.getById(id)
      .then(found => {
        if (found === null) {
          throw new Error('[NotFound]: Done item not found');
        }
        if (found.userId !== currentUserId) {
          throw new Error('[AccessDeined]: You don\'t have the permission to modify this item.');
        }
        return this._doneDynamoTableClient.update(id, Done.getModifiable(newData))
          .then(item => new Done(item.id, item.userId, item.doneThing, item.date));
      });
  }

}

module.exports = DoneRepository;
