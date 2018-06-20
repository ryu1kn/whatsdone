import Route = require('route-parser');
import LambdaRequestHandler from './LambdaRequestHandler';
import {ObjectMap} from './models/Collection';
import {Event} from './models/Lambda';
import {Response} from './models/Request';

export type RouteDefinition = {
  handler: LambdaRequestHandler['handle'],
  pattern: Route
};

export class Router {
  private _handlers: ObjectMap<RouteDefinition[]>;

  constructor() {
    this._handlers = {
      get: [],
      post: [],
      delete: [],
      put: []
    };
  }

  async route(event: Event): Promise<Response> {
    const method = event.httpMethod.toLowerCase();
    const handlerItem = this._handlers[method].find(
      handlerItem => !!handlerItem.pattern.match(event.path)
    );
    if (handlerItem) {
      const pathParameters = handlerItem.pattern.match(event.path);
      const finalEvent = Object.assign({}, event, {pathParameters});
      return handlerItem.handler(finalEvent);
    }
    return {
      statusCode: '404',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({
        errors: [
          {title: '404: Not Found'}
        ]
      })
    };
  }

  get(pattern: string, handler: LambdaRequestHandler['handle']) {
    this.registerHandler('get', pattern, handler);
  }

  post(pattern: string, handler: LambdaRequestHandler['handle']) {
    this.registerHandler('post', pattern, handler);
  }

  delete(pattern: string, handler: LambdaRequestHandler['handle']) {
    this.registerHandler('delete', pattern, handler);
  }

  put(pattern: string, handler: LambdaRequestHandler['handle']) {
    this.registerHandler('put', pattern, handler);
  }

  private registerHandler(method: string, pattern: string, handler: LambdaRequestHandler['handle']) {
    this._handlers[method].push({
      pattern: new Route(pattern),
      handler
    });
  }
}
