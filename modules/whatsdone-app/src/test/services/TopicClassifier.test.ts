import TopicClassifier from '../../lib/services/TopicClassifier';
import {deepStrictEqual} from 'assert';

describe('TopicClassifier', () => {
  it('classifies text and returns topics', async () => {
    const classifier = new TopicClassifier();
    const result = await classifier.classifyText('test');

    deepStrictEqual(result, []);
  });
});
