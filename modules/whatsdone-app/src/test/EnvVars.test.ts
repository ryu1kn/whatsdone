import {EnvVars} from '../lib/EnvVars';
import {deepStrictEqual, throws} from 'assert';
import ProcessEnv = NodeJS.ProcessEnv;

describe('Server EnvVars', () => {
  const env: ProcessEnv = {
    USER_POOL_ID: 'user-pool-id',
    WEBAPP_ORIGIN: 'webapp-origin',
    DB_REGION: 'db-region',
    DONE_TABLE_NAME: 'done-table-name',
    USER_ID_TABLE_NAME: 'user-id-table-name',
    COMPREHEND_REGION: 'comprehend-region',
    TOPIC_CLASSIFIER_ARN: 'topic-classifier-arn',
  };

  it('returns required environment variables', () => {
    const envVars = new EnvVars(env);
    deepStrictEqual(envVars.DB_REGION, 'db-region');
    deepStrictEqual(envVars.DONE_TABLE_NAME, 'done-table-name');
    deepStrictEqual(envVars.USER_ID_TABLE_NAME, 'user-id-table-name');
    deepStrictEqual(envVars.USER_POOL_ID, 'user-pool-id');
    deepStrictEqual(envVars.WEBAPP_ORIGIN, 'webapp-origin');
    deepStrictEqual(envVars.COMPREHEND_REGION, 'comprehend-region');
    deepStrictEqual(envVars.TOPIC_CLASSIFIER_ARN, 'topic-classifier-arn');
    deepStrictEqual(envVars.LOG_LEVEL, 'INFO');
  });

  it('returns specified log level', () => {
    const envVars = new EnvVars({...env, LOG_LEVEL: 'ERROR'});
    deepStrictEqual(envVars.LOG_LEVEL, 'ERROR');
  });

  it('errors out if required env vars are missing', () => {
    throws(() => {
      new EnvVars({} as ProcessEnv);  // tslint:disable-line:no-unused-expression
    });
  });
});
