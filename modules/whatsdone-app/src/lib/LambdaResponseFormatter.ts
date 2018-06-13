
import ServiceLocator from './ServiceLocator';

class LambdaResponseFormatter {
  private _config: any;

  constructor() {
    this._config = ServiceLocator.config;
  }

  format(response) {
    const headers = Object.assign({}, response.headers, {
      'Access-Control-Allow-Headers': 'Content-Type,X-Amz-Date,Authorization,X-Api-Key',
      'Access-Control-Allow-Methods': '*',
      'Access-Control-Allow-Credentials': 'true',
      'Access-Control-Allow-Origin': this._config.webappOrigin
    });
    return Object.assign({}, response, {headers});
  }

}

export = LambdaResponseFormatter;
