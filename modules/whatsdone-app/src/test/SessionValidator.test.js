
const ServiceLocator = require('../lib/ServiceLocator');
const SessionValidator = require('../lib/SessionValidator');

describe('Server SessionValidator', () => {

  it('tells that a session is valid', () => {
    ServiceLocator.load({
      createDateProvider: () => ({getCurrentDate: () => new Date('2017-03-29T12:00:00Z')})
    });
    const sessionValidator = new SessionValidator();
    const session = {createdAt: '2017-03-23T11:00:00Z'};
    expect(sessionValidator.validate(session)).to.be.true;
  });

  it('tells that a session is invalid if it is created more than a week ago', () => {
    ServiceLocator.load({
      createDateProvider: () => ({getCurrentDate: () => new Date('2017-03-29T12:00:00Z')})
    });
    const sessionValidator = new SessionValidator();
    const session = {createdAt: '2017-03-22T11:00:00Z'};
    expect(sessionValidator.validate(session)).to.be.false;
  });

  it('treats session not exists case as session is invalid', () => {
    const sessionValidator = new SessionValidator();
    const session = null;
    expect(sessionValidator.validate(session)).to.be.false;
  });

});
