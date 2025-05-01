import AWS = require('aws-sdk');
import ServiceFactory from './ServiceFactory';
import DoneRepository from './repositories/Done';
import UserIdRepository from './repositories/UserId';
import GetDonesCommand from './commands/GetDones';
import CreateDoneCommand from './commands/CreateDone';
import UpdateDoneCommand from './commands/UpdateDone';
import LambdaRequestHandler from './LambdaRequestHandler';
import {Logger} from './Logger';
import CognitoUserFinder from './CognitoUserFinder';
import RequestProcessErrorProcessor from './RequestProcessErrorProcessor';
import LambdaRequestNormaliser from './LambdaRequestNormaliser';
import LambdaResponseFormatter from './LambdaResponseFormatter';
import UserNameService from './services/UserNameService';
import DoneQueryHelper from './repositories/done-helpers/query';
import DynamoTableClient from './repositories/DynamoTableClient';
import {AppConfig} from './AppConfig';
import TopicClassifier from './services/TopicClassifier';

type InstanceCache = {
  Config?: AppConfig;
  CognitoIdentityServiceProvider?: AWS.CognitoIdentityServiceProvider;
  CognitoUserFinder?: CognitoUserFinder;
  GetDonesRequestHandler?: LambdaRequestHandler['handle'];
  PostDoneRequestHandler?: LambdaRequestHandler['handle'];
  DeleteDoneRequestHandler?: LambdaRequestHandler['handle'];
  UpdateDoneRequestHandler?: LambdaRequestHandler['handle'];
  CreateDoneCommand?: CreateDoneCommand;
  GetDonesCommand?: GetDonesCommand;
  UpdateDoneCommand?: UpdateDoneCommand;
  RequestProcessErrorProcessor?: RequestProcessErrorProcessor;
  LambdaRequestNormaliser?: LambdaRequestNormaliser;
  LambdaResponseFormatter?: LambdaResponseFormatter;
  Logger?: Logger;
  UserNameService?: UserNameService;
  UserIdRepository?: UserIdRepository;
  DoneRepository?: DoneRepository;
  DynamoDBDocumentClient?: AWS.DynamoDB.DocumentClient;
  ComprehendClient?: AWS.Comprehend;
  TranslateClient?: AWS.Translate;
  DoneQueryHelper?: DoneQueryHelper;
  DoneDynamoTableClient?: DynamoTableClient;
  UuidGenerator?: { generate: () => string };
  DateProvider?: { getCurrentDate: () => Date };
  TopicClassifier?: TopicClassifier;
};

class ServiceLocator {
  private serviceFactory!: ServiceFactory;
  private cache!: InstanceCache;

  load(serviceFactory: ServiceFactory) {
    this.serviceFactory = serviceFactory;
    this.cache = {};
  }

  get config() {
    this.cache.Config = this.cache.Config || this.serviceFactory.createConfig();
    return this.cache.Config!;
  }

  get cognitoIdentityServiceProvider() {
    this.cache.CognitoIdentityServiceProvider = this.cache.CognitoIdentityServiceProvider
        || this.serviceFactory.createCognitoIdentityServiceProvider();
    return this.cache.CognitoIdentityServiceProvider!;
  }

  get cognitoUserFinder() {
    this.cache.CognitoUserFinder = this.cache.CognitoUserFinder || this.serviceFactory.createCognitoUserFinder();
    return this.cache.CognitoUserFinder!;
  }

  get getDonesRequestHandler() {
    this.cache.GetDonesRequestHandler = this.cache.GetDonesRequestHandler
        || this.serviceFactory.createGetDonesRequestHandler();
    return this.cache.GetDonesRequestHandler!;
  }

  get postDoneRequestHandler() {
    this.cache.PostDoneRequestHandler = this.cache.PostDoneRequestHandler
        || this.serviceFactory.createPostDoneRequestHandler();
    return this.cache.PostDoneRequestHandler!;
  }

  get deleteDoneRequestHandler() {
    this.cache.DeleteDoneRequestHandler = this.cache.DeleteDoneRequestHandler
        || this.serviceFactory.createDeleteDoneRequestHandler();
    return this.cache.DeleteDoneRequestHandler!;
  }

  get updateDoneRequestHandler() {
    this.cache.UpdateDoneRequestHandler = this.cache.UpdateDoneRequestHandler
        || this.serviceFactory.createUpdateDoneRequestHandler();
    return this.cache.UpdateDoneRequestHandler!;
  }

  get createDoneCommand() {
    this.cache.CreateDoneCommand = this.cache.CreateDoneCommand || this.serviceFactory.createCreateDoneCommand();
    return this.cache.CreateDoneCommand!;
  }

  get getDonesCommand() {
    this.cache.GetDonesCommand = this.cache.GetDonesCommand || this.serviceFactory.createGetDonesCommand();
    return this.cache.GetDonesCommand!;
  }

  get updateDoneCommand() {
    this.cache.UpdateDoneCommand = this.cache.UpdateDoneCommand || this.serviceFactory.createUpdateDoneCommand();
    return this.cache.UpdateDoneCommand!;
  }

  get requestProcessErrorProcessor() {
    this.cache.RequestProcessErrorProcessor = this.cache.RequestProcessErrorProcessor
        || this.serviceFactory.createRequestProcessErrorProcessor();
    return this.cache.RequestProcessErrorProcessor!;
  }

  get lambdaRequestNormaliser() {
    this.cache.LambdaRequestNormaliser = this.cache.LambdaRequestNormaliser || this.serviceFactory.createLambdaRequestNormaliser();
    return this.cache.LambdaRequestNormaliser!;
  }

  get lambdaResponseFormatter() {
    this.cache.LambdaResponseFormatter = this.cache.LambdaResponseFormatter || this.serviceFactory.createLambdaResponseFormatter();
    return this.cache.LambdaResponseFormatter!;
  }

  get logger() {
    this.cache.Logger = this.cache.Logger || this.serviceFactory.createLogger();
    return this.cache.Logger!;
  }

  get userNameService() {
    this.cache.UserNameService = this.cache.UserNameService || this.serviceFactory.createUserNameService();
    return this.cache.UserNameService!;
  }

  get userIdRepository() {
    this.cache.UserIdRepository = this.cache.UserIdRepository || this.serviceFactory.createUserIdRepository();
    return this.cache.UserIdRepository!;
  }

  get doneRepository() {
    this.cache.DoneRepository = this.cache.DoneRepository || this.serviceFactory.createDoneRepository();
    return this.cache.DoneRepository!!;
  }

  get dynamoDBDocumentClient() {
    this.cache.DynamoDBDocumentClient = this.cache.DynamoDBDocumentClient || this.serviceFactory.createDynamoDBDocumentClient();
    return this.cache.DynamoDBDocumentClient!;
  }

  get comprehendClient() {
    this.cache.ComprehendClient = this.cache.ComprehendClient || this.serviceFactory.createComprehendClient();
    return this.cache.ComprehendClient!;
  }

  get translateClient() {
    this.cache.TranslateClient = this.cache.TranslateClient || this.serviceFactory.createTranslateClient();
    return this.cache.TranslateClient!;
  }

  get doneQueryHelper() {
    this.cache.DoneQueryHelper = this.cache.DoneQueryHelper || this.serviceFactory.createDoneQueryHelper();
    return this.cache.DoneQueryHelper!;
  }

  get doneDynamoTableClient() {
    this.cache.DoneDynamoTableClient = this.cache.DoneDynamoTableClient || this.serviceFactory.createDoneDynamoTableClient();
    return this.cache.DoneDynamoTableClient!;
  }

  get uuidGenerator() {
    this.cache.UuidGenerator = this.cache.UuidGenerator || this.serviceFactory.createUuidGenerator();
    return this.cache.UuidGenerator!;
  }

  get dateProvider() {
    this.cache.DateProvider = this.cache.DateProvider || this.serviceFactory.createDateProvider();
    return this.cache.DateProvider!;
  }

  get topicClassifier() {
    this.cache.TopicClassifier = this.cache.TopicClassifier || this.serviceFactory.createTopicClassifier();
    return this.cache.TopicClassifier!;
  }

}

export default new ServiceLocator();
