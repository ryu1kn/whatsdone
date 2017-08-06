
const ApiGatewayDeploymentHandler = require('./handler');

class ApiGatewayDeploymentHandlerFactory {

  constructor({fetch}) {
    this._fetch = fetch;
  }

  create() {
    return new ApiGatewayDeploymentHandler({fetch: this._fetch});
  }

}

module.exports = ApiGatewayDeploymentHandlerFactory;
