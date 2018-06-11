
const ServiceLocator = require('./lib/ServiceLocator');
const ServiceFactory = require('./lib/ServiceFactory');
const Route = require('route-parser');

ServiceLocator.load(new ServiceFactory({env: process.env}));

class Router {

  constructor() {
    this._handlers = {
      get: [],
      post: [],
      delete: [],
      put: []
    };
  }

  route(event, context, callback) {
    const method = event.httpMethod.toLowerCase();
    const handlerItem = this._handlers[method].find(
      handlerItem => handlerItem.pattern.match(event.path)
    );
    if (handlerItem) {
      const pathParameters = handlerItem.pattern.match(event.path);
      const finalEvent = Object.assign({}, event, {pathParameters});
      return handlerItem.handler(finalEvent, context, callback);
    }
    callback(null, {
      statusCode: '404',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({errors: [
        {title: '404: Not Found'}
      ]})
    });
  }

  get(pattern, handler) {
    this._registerHandler('get', pattern, handler);
  }

  post(pattern, handler) {
    this._registerHandler('post', pattern, handler);
  }

  delete(pattern, handler) {
    this._registerHandler('delete', pattern, handler);
  }

  put(pattern, handler) {
    this._registerHandler('put', pattern, handler);
  }

  _registerHandler(method, pattern, handler) {
    this._handlers[method].push({
      pattern: new Route(pattern),
      handler
    });
  }

}

const router = new Router();

router.get('/signout', ServiceLocator.signoutRequestHandler);
router.get('/dones', ServiceLocator.getDonesRequestHandler);
router.post('/dones', ServiceLocator.postDoneRequestHandler);
router.delete('/dones/:id', ServiceLocator.deleteDoneRequestHandler);
router.put('/dones/:id', ServiceLocator.updateDoneRequestHandler);

exports.handler = (...args) => router.route(...args);
