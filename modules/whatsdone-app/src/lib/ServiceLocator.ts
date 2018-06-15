import AWS = require('aws-sdk');
import ServiceFactory from './ServiceFactory';
import DoneRepository from './repositories/Done';
import UserIdRepository from './repositories/UserId';
import GetDonesCommand from './commands/GetDones';
import CreateDoneCommand from './commands/CreateDone';
import UpdateDoneCommand from './commands/UpdateDone';
import {ObjectMap} from './models/Collection';
import LambdaRequestHandler from './LambdaRequestHandler';
import {Logger} from './Logger';

class ServiceLocator {
  private _serviceFactory: ServiceFactory;
  private _cache: ObjectMap<Object>;

  load(serviceFactory) {
    this._serviceFactory = serviceFactory;
    this._cache = {};
  }

  get config(): {userPoolId: string, webappOrigin: string} {
    return this.get('config');
  }

  get cognitoIdentityServiceProvider() {
    return this.get('cognitoIdentityServiceProvider');
  }

  get cognitoUserFinder() {
    return this.get('cognitoUserFinder');
  }

  get encodedUrlParser() {
    return this.get('encodedUrlParser');
  }

  get faviconProvider() {
    return this.get('faviconProvider');
  }

  get jsonRequestBodyParser() {
    return this.get('jsonRequestBodyParser');
  }

  get staticContentsProvider() {
    return this.get('staticContentsProvider');
  }

  get getDonesRequestHandler(): LambdaRequestHandler['handle'] {
    return this.get('getDonesRequestHandler');
  }

  get postDoneRequestHandler(): LambdaRequestHandler['handle'] {
    return this.get('postDoneRequestHandler');
  }

  get deleteDoneRequestHandler(): LambdaRequestHandler['handle'] {
    return this.get('deleteDoneRequestHandler');
  }

  get updateDoneRequestHandler(): LambdaRequestHandler['handle'] {
    return this.get('updateDoneRequestHandler');
  }

  get createDoneCommand(): CreateDoneCommand {
    return this.get('createDoneCommand');
  }

  get deleteDoneCommand() {
    return this.get('deleteDoneCommand');
  }

  get getDonesCommand(): GetDonesCommand {
    return this.get('getDonesCommand');
  }

  get updateDoneCommand(): UpdateDoneCommand {
    return this.get('updateDoneCommand');
  }

  get requestProcessErrorProcessor() {
    return this.get('requestProcessErrorProcessor');
  }

  get lambdaRequestNormaliser() {
    return this.get('lambdaRequestNormaliser');
  }

  get lambdaResponseFormatter() {
    return this.get('lambdaResponseFormatter');
  }

  get logger(): Logger {
    return this.get('logger');
  }

  get userNameService() {
    return this.get('userNameService');
  }

  get userIdRepository(): UserIdRepository {
    return this.get('userIdRepository');
  }

  get doneRepository(): DoneRepository {
    return this.get('doneRepository');
  }

  get dynamoDBDocumentClient() {
    return this.get('dynamoDBDocumentClient') as AWS.DynamoDB.DocumentClient;
  }

  get doneQueryHelper() {
    return this.get('doneQueryHelper');
  }

  get doneDynamoTableClient() {
    return this.get('doneDynamoTableClient');
  }

  get uuidGenerator(): {generate: () => string} {
    return this.get('uuidGenerator');
  }

  get dateProvider(): {getCurrentDate: () => Date} {
    return this.get('dateProvider');
  }

  private get(serviceName) {
    const cachedInstance = this.getCachedInstance(serviceName);
    if (cachedInstance) return cachedInstance;

    const instance = this.getFromServiceFactory(serviceName);
    this.cacheInstance(serviceName, instance);
    return instance;
  }

  private getCachedInstance(serviceName) {
    return this._cache[this.getCacheName(serviceName)];
  }

  private getFromServiceFactory(serviceName) {
    const methodName = this.getFactoryName(serviceName);
    return this._serviceFactory[methodName]();
  }

  // fooBar -> getFooBar
  private getFactoryName(name) {
    return ['create', name[0].toUpperCase(), name.substring(1)].join('');
  }

  private cacheInstance(serviceName, serviceInstance) {
    const cacheName = this.getCacheName(serviceName);
    this._cache[cacheName] = serviceInstance;
  }

  private getCacheName(name) {
    return `_${name}`;
  }

}

export default new ServiceLocator();
