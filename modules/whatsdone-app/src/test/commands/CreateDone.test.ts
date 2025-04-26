import CreateDoneCommand from '../../lib/commands/CreateDone';
import ServiceLocator from '../../lib/ServiceLocator';
import ServiceFactory from '../../lib/ServiceFactory';
import * as td from 'testdouble';
import DoneRepository from '../../lib/repositories/Done';
import TopicClassifier from '../../lib/services/TopicClassifier';
import {deepStrictEqual} from 'assert';

describe('Server CreateDoneCommand', () => {
  it('returns list of dones with the names of their owners', async () => {
    const done = {doneThing: 'SOMETHING', date: 'DATE'};
    const doneWithUserId = Object.assign({}, done, {userId: 'USER_ID'});
    const doneRepository = td.object('write') as DoneRepository;
    td.when(doneRepository.write(doneWithUserId)).thenResolve(doneWithUserId);

    ServiceLocator.load({
      createDoneRepository: () => doneRepository
    } as ServiceFactory);
    const command = new CreateDoneCommand();

    const result = await command.execute(done, 'USER_ID');
    deepStrictEqual(result, {...doneWithUserId, topics: ["foo"]});
  });

  it('creates a done item with identified topics', async () => {
    const done = {doneThing: 'SOMETHING', date: 'DATE'};
    const finalDone = {
      ...done,
      userId: 'USER_ID',
      topics: ['TOPIC_FOO', 'TOPIC_BAR']
    };
    const doneRepository = td.object('write') as DoneRepository;
    const topicClassifier = td.object('classifyText') as TopicClassifier;

    td.when(doneRepository.write(finalDone)).thenResolve(finalDone);
    td.when(topicClassifier.classifyText('SOMETHING')).thenResolve(['TOPIC_FOO', 'TOPIC_BAR']);

    ServiceLocator.load({
      createDoneRepository: () => doneRepository,
      createTopicClassifier: () => topicClassifier
    } as ServiceFactory);
    const command = new CreateDoneCommand();

    const result = await command.execute(done, 'USER_ID');
    deepStrictEqual(result, finalDone);
  });

  it('handles topic identification failure gracefully', async () => {
    const done = {doneThing: 'SOMETHING', date: 'DATE'};
    const finalDone = {
      ...done,
      userId: 'USER_ID',
      topics: []
    };
    const doneRepository = td.object('write') as DoneRepository;
    const topicClassifier = td.object('classifyText') as TopicClassifier;

    td.when(doneRepository.write(finalDone)).thenResolve(finalDone);
    td.when(topicClassifier.classifyText('SOMETHING')).thenReject(new Error('Classification failed'));

    ServiceLocator.load({
      createDoneRepository: () => doneRepository,
      createTopicClassifier: () => topicClassifier
    } as ServiceFactory);
    const command = new CreateDoneCommand();

    const result = await command.execute(done, 'USER_ID');
    deepStrictEqual(result, finalDone);
  });
});

