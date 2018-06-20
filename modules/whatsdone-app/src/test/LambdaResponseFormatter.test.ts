import ServiceLocator from '../lib/ServiceLocator';
import LambdaResponseFormatter from '../lib/LambdaResponseFormatter';
import {expect} from './helper/TestUtils';
import ServiceFactory from '../lib/ServiceFactory';

describe('Server LambdaResponseFormatter', () => {

  it('passes through as standard response format IS lambda\'s response format', () => {
    const config = {webappOrigin: 'WEBAPP_ORIGIN'};
    ServiceLocator.load({
      createConfig: () => config
    } as ServiceFactory);
    const formatter = new LambdaResponseFormatter();

    const response = {
      statusCode: '200',
      headers: {'X-ANY-HEADER': '..'},
      NON_HEADER_VALUES: '..'
    };
    expect(formatter.format(response)).to.eql({
      statusCode: '200',
      headers: {
        'X-ANY-HEADER': '..',
        'Access-Control-Allow-Headers': 'Content-Type,X-Amz-Date,Authorization,X-Api-Key',
        'Access-Control-Allow-Methods': '*',
        'Access-Control-Allow-Credentials': 'true',
        'Access-Control-Allow-Origin': 'WEBAPP_ORIGIN'
      },
      NON_HEADER_VALUES: '..'
    });
  });

});
