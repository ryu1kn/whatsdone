
const AWS = require('aws-sdk');

const cloudfront = new AWS.CloudFront();

const params = {
  CloudFrontOriginAccessIdentityConfig: {
    CallerReference: '1',
    Comment: "What's Done"
  }
};
cloudfront.createCloudFrontOriginAccessIdentity(params).promise()
  .then(data => {
    const output = {CloudFrontOriginAccessIdentity: data.CloudFrontOriginAccessIdentity};
    console.log(JSON.stringify(output, null, 2));
  });
