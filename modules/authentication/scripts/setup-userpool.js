const AWS = require('aws-sdk');

const cognitoidentityserviceprovider = new AWS.CognitoIdentityServiceProvider({
  region: 'ap-southeast-2'
});

updateUserPoolClient()
  .then(result => {
    console.log(result);
  })
  .catch(e => {
    console.error(e.stack);
  });

function createUserPoolDomain() {
  const params = {
    Domain: 'whatsdone-dev-ryuichi',
    UserPoolId: 'ap-southeast-2_R0tZ3qQMb'
  };
  return cognitoidentityserviceprovider.createUserPoolDomain(params).promise();
}

function updateUserPoolClient() {
  var params = {
    ClientId: '2ufgt2tvstsm8qvctfqm40p7fk',
    UserPoolId: 'ap-southeast-2_R0tZ3qQMb',
    AllowedOAuthFlows: ['implicit'],
    AllowedOAuthFlowsUserPoolClient: false,
    AllowedOAuthScopes: ['openid'],
    // CallbackURLs: ['https://requestb.in/1bfd5891'],
    // ClientName: 'Webapp',
    // DefaultRedirectURI: 'https://requestb.in/1bfd5891',
    // LogoutURLs: ['https://requestb.in/1bfd5891'],
    CallbackURLs: ['https://whatsdone.ryuichi.io'],
    ClientName: 'Webapp',
    DefaultRedirectURI: 'https://whatsdone.ryuichi.io',
    LogoutURLs: ['https://whatsdone.ryuichi.io'],
    ReadAttributes: [],
    RefreshTokenValidity: 1,
    SupportedIdentityProviders: ['COGNITO'],
    WriteAttributes: []
  };
  return cognitoidentityserviceprovider.updateUserPoolClient(params).promise();
}
