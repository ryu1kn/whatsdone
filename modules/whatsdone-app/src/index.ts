import ServiceLocator from './lib/ServiceLocator';
import ServiceFactory from './lib/ServiceFactory';
import Route = require('route-parser');
import {ObjectMap} from './lib/models/Collection';
import LambdaRequestHandler from './lib/LambdaRequestHandler';

ServiceLocator.load(new ServiceFactory(process.env));

type RouteDefinition = {
  handler: LambdaRequestHandler['handle'],
  pattern: Route
};

class Router {
  private _handlers: ObjectMap<RouteDefinition[]>;

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
    this.registerHandler('get', pattern, handler);
  }

  post(pattern, handler) {
    this.registerHandler('post', pattern, handler);
  }

  delete(pattern, handler) {
    this.registerHandler('delete', pattern, handler);
  }

  put(pattern, handler) {
    this.registerHandler('put', pattern, handler);
  }

  private registerHandler(method, pattern, handler) {
    this._handlers[method].push({
      pattern: new Route(pattern),
      handler
    });
  }

}

const router = new Router();

router.get('/dones', ServiceLocator.getDonesRequestHandler);
router.post('/dones', ServiceLocator.postDoneRequestHandler);
router.delete('/dones/:id', ServiceLocator.deleteDoneRequestHandler);
router.put('/dones/:id', ServiceLocator.updateDoneRequestHandler);

export const handler = (event, context, callback) => router.route(event, context, callback);
