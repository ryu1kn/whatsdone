
class WrappedError extends Error {

  constructor(originalError) {
    super(originalError.message);
    this.name = 'WrappedError';
    this.originalError = originalError;
  }

}

module.exports = WrappedError;
