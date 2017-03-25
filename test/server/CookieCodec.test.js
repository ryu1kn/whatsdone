
const CookieCodec = require('../../src/server/CookieCodec');

describe('Server CookieCodec', () => {

  it('encodes session id into predefined cookie string', () => {
    const cookieCodec = new CookieCodec({signatureSecret: 'COOKIE_SIGN_SECRET'});
    const cookie = cookieCodec.encode({sessionId: 'SESSION_ID'});
    expect(cookie).to.eql('connect.sid=s%3ASESSION_ID.kOYvjX8Ze3rKgmCikCRPJw9BrCk23jgQGHyANm0ItgI; Path=/; HttpOnly');
  });

});
