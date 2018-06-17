import ServiceLocator from './lib/ServiceLocator';
import ServiceFactory from './lib/ServiceFactory';
import {Event} from './lib/models/Lambda';
import {Router} from './lib/Router';

ServiceLocator.load(new ServiceFactory(process.env));

const router = new Router();
router.get('/dones', ServiceLocator.getDonesRequestHandler);
router.post('/dones', ServiceLocator.postDoneRequestHandler);
router.delete('/dones/:id', ServiceLocator.deleteDoneRequestHandler);
router.put('/dones/:id', ServiceLocator.updateDoneRequestHandler);

export const handler = (event: Event, context: any) => router.route(event);
