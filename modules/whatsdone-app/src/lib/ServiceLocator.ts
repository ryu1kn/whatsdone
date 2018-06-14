import AWS = require('aws-sdk');
import ServiceFactory from './ServiceFactory';
import DoneRepository from './repositories/Done';
import UserIdRepository from './repositories/UserId';
import DeleteDoneRequestHandler from './request-processors/DeleteDone';
import GetDonesRequestProcessor from './request-processors/GetDones';
import GetDonesCommand from './commands/GetDones';
import CreateDoneCommand from './commands/CreateDone';
import UpdateDoneCommand from './commands/UpdateDone';
import {ObjectMap} from './models/Collection';
import LambdaRequestHandler from './LambdaRequestHandler';

class ServiceLocator {
  private _serviceFactory: ServiceFactory;
  private _cache: ObjectMap<Object>;

  load(serviceFactory) {
    this._serviceFactory = serviceFactory;
    this._cache = {};
  }

  get config(): {userPoolId: string, webappOrigin: string} {
    return this._get('config');
  }

  get accessLogger() {
    return this._get('accessLogger');
  }

  get cognitoIdentityServiceProvider() {
    return this._get('cognitoIdentityServiceProvider');
  }

  get cognitoUserFinder() {
    return this._get('cognitoUserFinder');
  }

  get encodedUrlParser() {
    return this._get('encodedUrlParser');
  }

  get faviconProvider() {
    return this._get('faviconProvider');
  }

  get jsonRequestBodyParser() {
    return this._get('jsonRequestBodyParser');
  }

  get staticContentsProvider() {
    return this._get('staticContentsProvider');
  }

  get getDonesRequestHandler(): LambdaRequestHandler['handle'] {
    return this._get('getDonesRequestHandler');
  }

  get postDoneRequestHandler(): LambdaRequestHandler['handle'] {
    return this._get('postDoneRequestHandler');
  }

  get deleteDoneRequestHandler(): LambdaRequestHandler['handle'] {
    return this._get('deleteDoneRequestHandler');
  }

  get updateDoneRequestHandler(): LambdaRequestHandler['handle'] {
    return this._get('updateDoneRequestHandler');
  }

  get createDoneCommand(): CreateDoneCommand {
    return this._get('createDoneCommand');
  }

  get deleteDoneCommand() {
    return this._get('deleteDoneCommand');
  }

  get getDonesCommand(): GetDonesCommand {
    return this._get('getDonesCommand');
  }

  get updateDoneCommand(): UpdateDoneCommand {
    return this._get('updateDoneCommand');
  }

  get requestProcessErrorProcessor() {
    return this._get('requestProcessErrorProcessor');
  }

  get lambdaRequestNormaliser() {
    return this._get('lambdaRequestNormaliser');
  }

  get lambdaResponseFormatter() {
    return this._get('lambdaResponseFormatter');
  }

  get logger(): {error: (...args: any[]) => void} {
    return this._get('logger');
  }

  get userNameService() {
    return this._get('userNameService');
  }

  get userIdRepository(): UserIdRepository {
    return this._get('userIdRepository');
  }

  get doneRepository(): DoneRepository {
    return this._get('doneRepository');
  }

  get dynamoDBDocumentClient() {
    return this._get('dynamoDBDocumentClient') as AWS.DynamoDB.DocumentClient;
  }

  get doneQueryHelper() {
    return this._get('doneQueryHelper');
  }

  get doneDynamoTableClient() {
    return this._get('doneDynamoTableClient');
  }

  get uuidGenerator(): {generate: () => string} {
    return this._get('uuidGenerator');
  }

  get dateProvider(): {getCurrentDate: () => Date} {
    return this._get('dateProvider');
  }

  _get(serviceName) {
    const cachedInstance = this._getCachedInstance(serviceName);
    if (cachedInstance) return cachedInstance;

    const instance = this._getFromServiceFactory(serviceName);
    this._cacheInstance(serviceName, instance);
    return instance;
  }

  _getCachedInstance(serviceName) {
    return this._cache[this._getCacheName(serviceName)];
  }

  _getFromServiceFactory(serviceName) {
    const methodName = this._getFactoryName(serviceName);
    return this._serviceFactory[methodName]();
  }

  // fooBar -> getFooBar
  _getFactoryName(name) {
    return ['create', name[0].toUpperCase(), name.substring(1)].join('');
  }

  _cacheInstance(serviceName, serviceInstance) {
    const cacheName = this._getCacheName(serviceName);
    this._cache[cacheName] = serviceInstance;
  }

  _getCacheName(name) {
    return `_${name}`;
  }

}

export default new ServiceLocator();