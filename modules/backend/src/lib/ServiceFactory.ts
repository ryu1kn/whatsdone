import AWSXRay = require('aws-xray-sdk');
import LambdaRequestHandler from './LambdaRequestHandler';
import {randomUUID} from 'node:crypto';
import {CognitoIdentityProviderClient} from '@aws-sdk/client-cognito-identity-provider';
import {DynamoDBClient} from '@aws-sdk/client-dynamodb';
import {DynamoDBDocumentClient} from '@aws-sdk/lib-dynamodb';
import {Logger, LogLevelString} from './Logger';
import {EnvVars} from './EnvVars';

// The AWS SDK v3 is modular, so X-Ray can no longer wrap the whole SDK as it
// did for v2 via captureAWS. Each client is instrumented individually with
// captureAWSv3Client instead. Tracing is skipped for local runs, where there
// is no X-Ray segment to attach sub-segments to.
const isLocalRun = process.env.IS_LOCAL_RUN === 'true';

function captureClient<T>(client: T): T {
  return isLocalRun ? client : AWSXRay.captureAWSv3Client(client);
}

export default class ServiceFactory {
  private env: EnvVars;

  constructor(env: EnvVars) {
    this.env = env;
  }

  createConfig() {
    return {
      userPoolId: this.env.USER_POOL_ID,
      webappOrigin: this.env.WEBAPP_ORIGIN
    };
  }

  createCognitoIdentityServiceProvider() {
    return captureClient(new CognitoIdentityProviderClient({}));
  }

  createCognitoUserFinder() {
    const CognitoUserFinder = require('./CognitoUserFinder').default;
    return new CognitoUserFinder();
  }

  createGetDonesRequestHandler(): LambdaRequestHandler['handle'] {
    const GetDonesRequestProcessor = require('./request-processors/GetDones').default;
    const requestHandler = new LambdaRequestHandler(new GetDonesRequestProcessor());
    return requestHandler.handle;
  }

  createPostDoneRequestHandler(): LambdaRequestHandler['handle'] {
    const PostDoneRequestProcessor = require('./request-processors/PostDone').default;
    const requestHandler = new LambdaRequestHandler(new PostDoneRequestProcessor());
    return requestHandler.handle;
  }

  createDeleteDoneRequestHandler(): LambdaRequestHandler['handle'] {
    const DeleteDoneRequestProcessor = require('./request-processors/DeleteDone').default;
    const requestHandler = new LambdaRequestHandler(new DeleteDoneRequestProcessor());
    return requestHandler.handle;
  }

  createUpdateDoneRequestHandler(): LambdaRequestHandler['handle'] {
    const UpdateDoneRequestProcessor = require('./request-processors/UpdateDone').default;
    const requestHandler = new LambdaRequestHandler(new UpdateDoneRequestProcessor());
    return requestHandler.handle;
  }

  createCreateDoneCommand() {
    const CreateDoneCommand = require('./commands/CreateDone').default;
    return new CreateDoneCommand();
  }

  createGetDonesCommand() {
    const GetDonesCommand = require('./commands/GetDones').default;
    return new GetDonesCommand();
  }

  createUpdateDoneCommand() {
    const UpdateDoneCommand = require('./commands/UpdateDone').default;
    return new UpdateDoneCommand();
  }

  createRequestProcessErrorProcessor() {
    const RequestProcessErrorProcessor = require('./RequestProcessErrorProcessor').default;
    return new RequestProcessErrorProcessor();
  }

  createLambdaRequestNormaliser() {
    const LambdaRequestNormaliser = require('./LambdaRequestNormaliser').default;
    return new LambdaRequestNormaliser();
  }

  createLambdaResponseFormatter() {
    const LambdaResponseFormatter = require('./LambdaResponseFormatter').default;
    return new LambdaResponseFormatter();
  }

  createLogger(): Logger {
    return new Logger(this.env.LOG_LEVEL as LogLevelString);
  }

  createDynamoDBDocumentClient(): DynamoDBDocumentClient {
    const client = new DynamoDBClient({region: this.env.DB_REGION});
    return captureClient(DynamoDBDocumentClient.from(client, {
      marshallOptions: {removeUndefinedValues: true}
    }));
  }

  createDoneQueryHelper() {
    const DoneQueryHelper = require('./repositories/done-helpers/query').default;
    return new DoneQueryHelper(this.env.DONE_TABLE_NAME);
  }

  createDoneDynamoTableClient() {
    const DynamoTableClient = require('./repositories/DynamoTableClient').default;
    return new DynamoTableClient(this.env.DONE_TABLE_NAME, 'id');
  }

  createDoneRepository() {
    const DoneRepository = require('./repositories/Done').default;
    return new DoneRepository();
  }

  createUserNameService() {
    const UserNameService = require('./services/UserNameService').default;
    return new UserNameService();
  }

  createUserIdRepository() {
    const UserIdRepository = require('./repositories/UserId').default;
    return new UserIdRepository(this.env.USER_ID_TABLE_NAME);
  }

  createUuidGenerator(): {generate: () => string} {
    return {generate: () => randomUUID()};
  }

  createDateProvider(): {getCurrentDate: () => Date} {
    return {getCurrentDate: () => new Date()};
  }

  createTopicClassifier() {
    const TopicClassifier = require('./services/TopicClassifier').default;
    return new TopicClassifier();
  }

}
