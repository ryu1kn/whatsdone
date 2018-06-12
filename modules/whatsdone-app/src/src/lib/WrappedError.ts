
class WrappedError extends Error {
  public originalError: any;
  public details: any;

  constructor(originalError, errorDetails) {
    super(originalError.message);
    this.name = 'WrappedError';
    this.originalError = originalError;
    this.details = errorDetails;
  }

}

export = WrappedError;
