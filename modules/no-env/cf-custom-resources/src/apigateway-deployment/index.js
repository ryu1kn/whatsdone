
const fetch = require('node-fetch');
const ApiGatewayDeploymentHandlerFactory = require('./lib/handler');
const apiGatewayDeploymentHandler = new ApiGatewayDeploymentHandlerFactory({fetch}).create();

module.exports = (...args) => apiGatewayDeploymentHandler.handle(...args);
