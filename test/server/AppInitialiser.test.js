
const AppInitialiser = require('../../src/server/AppInitialiser');
const ServiceLocator = require('../../src/server/ServiceLocator');

describe('Server AppInitialiser', () => {

  it('initialises express app with variables & middlewares', () => {
    ServiceLocator.load({
      createViewDirectoryPath: () => 'ViewDirectoryPath',
      createAccessLogger: () => 'AccessLogger',
      createJsonRequestBodyParser: () => 'JsonRequestBodyParser',
      createEncodedUrlParser: () => 'EncodedUrlParser',
      createCookieParser: () => 'CookieParser',
      createStaticContentsProvider: () => 'StaticContentsProvider',
      createFaviconProvider: () => 'FaviconProvider',
      createSessionManager: () => 'SessionManager',
      createAuthBasedRedirectMiddleware: () => 'AuthBasedRedirectMiddleware',
      createGetRootPageRequestHandler: () => 'GetRootPageRequestHandler',
      createGetSigninRequestHandler: () => 'GetSigninRequestHandler',
      createPostSigninRequestHandler: () => 'PostSigninRequestHandler',
      createSignoutRequestHandler: () => 'SignoutRequestHandler',
      createGetDonesRequestHandler: () => 'GetDonesRequestHandler',
      createPostDoneRequestHandler: () => 'PostDoneRequestHandler',
      createDeleteDoneRequestHandler: () => 'DeleteDoneRequestHandler',
      createUpdateDoneRequestHandler: () => 'UpdateDoneRequestHandler',
      createNoMatchingRouteRequestHandler: () => 'NoMatchingRouteRequestHandler'
    });
    const initiliser = new AppInitialiser();
    const app = {
      set: sinon.spy(),
      use: sinon.spy(),
      all: sinon.spy(),
      get: sinon.spy(),
      put: sinon.spy(),
      post: sinon.spy(),
      delete: sinon.spy()
    };
    initiliser.initialise(app);

    expect(app.set.args).to.eql([
      ['views', 'ViewDirectoryPath']
    ]);
    expect(app.use.args).to.eql([
      ['AccessLogger'],
      ['JsonRequestBodyParser'],
      ['EncodedUrlParser'],
      ['CookieParser'],
      ['StaticContentsProvider'],
      ['FaviconProvider'],
      ['SessionManager']
    ]);
    expect(app.all.args).to.eql([
      ['*', 'NoMatchingRouteRequestHandler']
    ]);
    expect(app.get.args).to.eql([
      ['/', 'GetRootPageRequestHandler'],
      ['/signin', 'GetSigninRequestHandler'],
      ['/signout', 'SignoutRequestHandler'],
      ['/dones.json', 'GetDonesRequestHandler']
    ]);
    expect(app.post.args).to.eql([
      ['/signin', 'PostSigninRequestHandler'],
      ['/signout', 'SignoutRequestHandler'],
      ['/dones.json', 'PostDoneRequestHandler']
    ]);
    expect(app.delete.args).to.eql([
      ['/dones.json/:id', 'DeleteDoneRequestHandler']
    ]);
    expect(app.put.args).to.eql([
      ['/dones.json/:id', 'UpdateDoneRequestHandler']
    ]);
  });

});
