
import chai = require('chai');
chai.use(require('sinon-chai'));

export const expect = chai.expect;
export const sinon = require('sinon');

export const throwError = () => {
  throw new Error('Should not have been called');
};

// stubWithArgs([arg11, arg12, ...], return1, [arg21, ...], return2)
export const stubWithArgs = function (...args) {
  const stub = sinon.stub();
  for (let i = 0; i + 1 < args.length; i += 2) {
    stub.withArgs.apply(stub, args[i]).returns(args[i + 1]);
  }
  return stub;
};

// stubReturns(return1, return2, ...)
export const stubReturns = function (...args) {
  return args.reduce((stub, arg, index) => {
    stub.onCall(index).returns(arg);
    return stub;
  }, sinon.stub());
};
