
class AuthTokenProvider {

  setTokens(accessToken) {
    this._accessToken = accessToken;
  }

  getIdToken() {
    return this._accessToken.getIdToken().getJwtToken();
  }

}

export default AuthTokenProvider;
