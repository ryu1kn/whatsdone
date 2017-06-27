
const ServiceLocator = require('./lib/ServiceLocator');
const ServiceFactory = require('./lib/ServiceFactory');

ServiceLocator.load(new ServiceFactory({env: process.env}));

exports.getRootPageRequestHandler = ServiceLocator.getRootPageRequestHandler;

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
    const handlerItem = this._handlers[method].find(handlerItem => handlerItem.pattern === event.path); // XXX: Incomplete
    if (handlerItem) return handlerItem.handler(event, context, callback);
    callback(null, {
      statusCode: '404',
      headers: {'Content-Type': 'text/html'},
      body: 'Not Found'
    });
  }

  get(pattern, handler) {
    this._handlers.get.push({pattern, handler});
  }

  post(pattern, handler) {
    this._handlers.post.push({pattern, handler});
  }

  delete(pattern, handler) {
    this._handlers.delete.push({pattern, handler});
  }

  put(pattern, handler) {
    this._handlers.put.push({pattern, handler});
  }

}

const router = new Router();

router.get('/signin', ServiceLocator.getSigninRequestHandler);
router.post('/signin', ServiceLocator.postSigninRequestHandler);
router.get('/signout', ServiceLocator.signoutRequestHandler);
router.get('/dones', ServiceLocator.getDonesRequestHandler);
router.post('/dones', ServiceLocator.postDoneRequestHandler);
router.delete('/dones/:id', ServiceLocator.deleteDoneRequestHandler);
router.put('/dones/:id', ServiceLocator.updateDoneRequestHandler);

exports.handler = (...args) => router.route(...args);
