import 'bootstrap/dist/css/bootstrap.css';
import 'bootstrap/dist/js/bootstrap.min';

import {createRoot} from 'react-dom/client';
import {BrowserRouter as Router, Route, Routes} from 'react-router-dom';
import {Provider} from 'react-redux';
import {createStore} from 'redux';

import ServiceLocator from './service-locator';
import ServiceFactory from './service-factory';
import reducer from './reducer';
import DonePage from './done/components/page';

ServiceLocator.load(new ServiceFactory());

const store = createStore(
  reducer,
  window.__REDUX_DEVTOOLS_EXTENSION__?.()
);

// Initialize features based on URL parameters
const feature = new URLSearchParams(window.location.search).get('feature');
store.dispatch({
  type: 'INIT_FEATURES',
  features: feature ? [feature] : []
});

ServiceLocator.cognitoUserInitialiser.initialise()
  .then(() => {
    window.history.replaceState({}, '', window.location.pathname + window.location.search);
  })
  .then(() => store.dispatch({type: 'API_READY'}))
  .catch((e: Error) => { console.error(e.stack); });

createRoot(document.getElementById('content')!).render(
  (
    <Provider store={store}>
      <Router>
        <Routes>
          <Route path="/" element={<DonePage />} />
        </Routes>
      </Router>
    </Provider>
  )
);
