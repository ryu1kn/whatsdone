
const handlerMap = Object.create(null);

exports.handler = createHandler('handler', './sample');
exports.apiGatewayDeploymentHandler = createHandler('apiGatewayDeploymentHandler', './apigateway-deployment');

function createHandler(handlerId, handlerPath) {
  return (...args) => {
    try {
      if (!handlerMap[handlerId]) {
        handlerMap[handlerId] = require(handlerPath);
      }
      handlerMap[handlerId](...args);
    } catch (e) {
      callback(e);
    }
  };
}
