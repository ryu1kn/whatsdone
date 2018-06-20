import {expect} from './helper/TestUtils';
import {EnvVars} from '../lib/EnvVars';
import ProcessEnv = NodeJS.ProcessEnv;

describe('Server EnvVars', () => {
  it('returns required environment variables', () => {
    const env: ProcessEnv = {
      USER_POOL_ID: 'user-pool-id',
      WEBAPP_ORIGIN: 'webapp-origin',
      DB_REGION: 'db-region',
      DONE_TABLE_NAME: 'done-table-name',
      USER_ID_TABLE_NAME: 'user-id-table-name'
    };
    const envVars = new EnvVars(env);
    expect(envVars.DB_REGION).to.eql('db-region');
    expect(envVars.DONE_TABLE_NAME).to.eql('done-table-name');
    expect(envVars.USER_ID_TABLE_NAME).to.eql('user-id-table-name');
    expect(envVars.USER_POOL_ID).to.eql('user-pool-id');
    expect(envVars.WEBAPP_ORIGIN).to.eql('webapp-origin');
  });

  it('errors out if required env vars are missing', () => {
    expect(() => {
      new EnvVars({} as ProcessEnv);  // tslint:disable-line:no-unused-expression
    }).throws();
  });
});
