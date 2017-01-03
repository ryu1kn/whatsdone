
const ServiceLocator = require('../../../src/server/ServiceLocator');

// This is empty test file to include routes/Dones.js for code coverage report

ServiceLocator.load({
  getDoneRepository: () => {},
  getUserRepository: () => {}
});

require('../../../src/server/routes/Dones');
