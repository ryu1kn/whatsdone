
const SessionValidator = require('../../src/server/SessionValidator');

describe('Server SessionValidator', () => {

  it('tells that a session is valid', () => {
    const sessionValidator = new SessionValidator();
    const session = {isAuthorized: true};
    expect(sessionValidator.validate(session)).to.be.true;
  });

  it('tells that a session is invalid', () => {
    const sessionValidator = new SessionValidator();
    const session = {isAuthorized: false};
    expect(sessionValidator.validate(session)).to.be.false;
  });

  it('treats session not exists case as session is invalid', () => {
    const sessionValidator = new SessionValidator();
    const session = null;
    expect(sessionValidator.validate(session)).to.be.false;
  });

});
