
'use strict';

const ServiceLocator = require('./ServiceLocator');

class AppInitialiser {

  initialise(app) {
    app.set('views', ServiceLocator.viewDirectoryPath);

    app.use(ServiceLocator.accessLogger);
    app.use(ServiceLocator.jsonRequestBodyParser);
    app.use(ServiceLocator.encodedUrlParser);
    app.use(ServiceLocator.staticContentsProvider);
    app.use(ServiceLocator.faviconProvider);

    app.get('/', ServiceLocator.getRootPageRequestHandler);
    app.get('/signin', ServiceLocator.getSigninRequestHandler);
    app.post('/signin', ServiceLocator.postSigninRequestHandler);
    app.get('/signout', ServiceLocator.signoutRequestHandler);
    app.post('/signout', ServiceLocator.signoutRequestHandler);
    app.get('/dones.json', ServiceLocator.getDonesRequestHandler);
    app.post('/dones.json', ServiceLocator.postDoneRequestHandler);
    app.delete('/dones.json/:id', ServiceLocator.deleteDoneRequestHandler);
    app.put('/dones.json/:id', ServiceLocator.updateDoneRequestHandler);
    app.all('*', ServiceLocator.noMatchingRouteRequestHandler);
  }

}

module.exports = AppInitialiser;
