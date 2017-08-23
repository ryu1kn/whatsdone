
class WrappedError extends Error {

  constructor(originalError, errorDetails) {
    super(originalError.message);
    this.name = 'WrappedError';
    this.originalError = originalError;
    this.details = errorDetails;
  }

}

module.exports = WrappedError;
