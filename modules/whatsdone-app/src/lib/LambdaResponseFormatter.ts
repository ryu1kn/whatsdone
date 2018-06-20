import ServiceLocator from './ServiceLocator';
import {AppConfig} from './AppConfig';
import {Response} from './models/Request';

export default class LambdaResponseFormatter {
  private _config: AppConfig;

  constructor() {
    this._config = ServiceLocator.config;
  }

  format(response: Response) {
    const headers = Object.assign({}, response.headers, {
      'Access-Control-Allow-Headers': 'Content-Type,X-Amz-Date,Authorization,X-Api-Key',
      'Access-Control-Allow-Methods': '*',
      'Access-Control-Allow-Credentials': 'true',
      'Access-Control-Allow-Origin': this._config.webappOrigin
    });
    return Object.assign({}, response, {headers});
  }

}
