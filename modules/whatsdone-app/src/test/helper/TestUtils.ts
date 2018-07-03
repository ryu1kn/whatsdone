
import chai = require('chai');
chai.use(require('sinon-chai'));

export const expect = chai.expect;

export const throwError = () => {
  throw new Error('Should not have been called');
};
