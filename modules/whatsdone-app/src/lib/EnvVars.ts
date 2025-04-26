import ProcessEnv = NodeJS.ProcessEnv;

export class EnvVars {
  private static REQUIRED_VARS = ['USER_POOL_ID', 'WEBAPP_ORIGIN', 'DB_REGION', 'DONE_TABLE_NAME', 'USER_ID_TABLE_NAME', 'COMPREHEND_REGION', 'TOPIC_CLASSIFIER_ARN'];

  private env: ProcessEnv;

  constructor(env: ProcessEnv) {
    this.validate(env);
    this.env = env;
  }

  get USER_POOL_ID(): string {
    return this.env.USER_POOL_ID!;
  }

  get WEBAPP_ORIGIN(): string {
    return this.env.WEBAPP_ORIGIN!;
  }

  get DB_REGION(): string {
    return this.env.DB_REGION!;
  }

  get DONE_TABLE_NAME(): string {
    return this.env.DONE_TABLE_NAME!;
  }

  get USER_ID_TABLE_NAME(): string {
    return this.env.USER_ID_TABLE_NAME!;
  }

  get COMPREHEND_REGION(): string {
    return this.env.COMPREHEND_REGION!;
  }

  get TOPIC_CLASSIFIER_ARN(): string {
    return this.env.TOPIC_CLASSIFIER_ARN!;
  }

  get LOG_LEVEL(): string {
    return this.env.LOG_LEVEL || 'INFO';
  }

  private validate(env: NodeJS.ProcessEnv) {
    const missingVars = EnvVars.REQUIRED_VARS.filter(v => !env[v]);
    if (missingVars.length !== 0) throw new Error(`Environment variables missing: ${missingVars}`);
  }
}
