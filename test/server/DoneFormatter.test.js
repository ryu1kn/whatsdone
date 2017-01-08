
const DoneFormatter = require('../../src/server/DoneFormatter');

describe('Server DoneFormatter', () => {

  it('formats done data for client use', () => {
    const formatter = new DoneFormatter();
    const done = {
      doneThing: 'DONE_THING',
      date: 'DATE',
      id: 'ID',
      userId: 'USER_ID',
      INTERNAL_DATA: '..'
    };
    expect(formatter.format(done)).to.eql('{"id":"ID","userId":"USER_ID","date":"DATE","doneThing":"DONE_THING"}');
  });

});
