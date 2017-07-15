
const CookieCodec = require('../lib/CookieCodec');

describe('Server CookieCodec', () => {

  it('encodes session id into predefined cookie string', () => {
    const cookieCodec = new CookieCodec({signatureSecret: 'COOKIE_SIGN_SECRET'});
    const cookie = cookieCodec.encode({sessionId: 'SESSION_ID'});
    expect(cookie).to.eql('connect.sid=s%3ASESSION_ID.kOYvjX8Ze3rKgmCikCRPJw9BrCk23jgQGHyANm0ItgI; Path=/; HttpOnly');
  });

  it('extracts session id from a cookie string', () => {
    const cookieCodec = new CookieCodec({signatureSecret: 'COOKIE_SIGN_SECRET'});
    const sessionId = cookieCodec.extractSessionId('connect.sid=s%3ASESSION_ID.kOYvjX8Ze3rKgmCikCRPJw9BrCk23jgQGHyANm0ItgI; Path=/; HttpOnly');
    expect(sessionId).to.eql('SESSION_ID');
  });

  it('returns null as session id if cookie string is not given', () => {
    const cookieCodec = new CookieCodec({signatureSecret: 'COOKIE_SIGN_SECRET'});
    const sessionId = cookieCodec.extractSessionId(undefined);
    expect(sessionId).to.eql(null);
  });

});
