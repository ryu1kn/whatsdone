const AWS = require('aws-sdk');

const cognitoidentityserviceprovider = new AWS.CognitoIdentityServiceProvider();
const params = {
  Domain: 'whatsdone-dev-ryuichi',
  UserPoolId: 'ap-southeast-2_R0tZ3qQMb'
};
cognitoidentityserviceprovider
  .createUserPoolDomain(params)
  .promise()
  .then(result => {
    console.log(result);
  })
  .catch(e => {
    console.error(e.stack);
  });
