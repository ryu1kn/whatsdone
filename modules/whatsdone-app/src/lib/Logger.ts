
enum LogLevel {
  DEBUG,
  INFO,
  WARN,
  ERROR,
}

export type LogLevelString = keyof typeof LogLevel;

export class Logger {
  private _logLevel: LogLevel;

  constructor(ls: LogLevelString) {
    this._logLevel = LogLevel[ls] ?? LogLevel.INFO
    console.info(`Logger initialized with level: ${LogLevel[this._logLevel]}`);
  }

  error(...args: any[]): void {
    if (this._logLevel >= LogLevel.ERROR) {
      console.error(...args);
    }
  }

  warn(...args: any[]): void {
    if (this._logLevel >= LogLevel.WARN) {
      console.warn(...args);
    }
  }

  info(...args: any[]): void {
    if (this._logLevel >= LogLevel.INFO) {
      console.info(...args);
    }
  }

  debug(...args: any[]): void {
    if (this._logLevel >= LogLevel.DEBUG) {
      console.debug(...args);
    }
  }
}
