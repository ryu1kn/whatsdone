
export const MONTH_LENGTH = 'YYYY-MM'.length;

// Copies only the given keys that are actually present on the source,
// matching lodash.pick semantics (absent keys are not added as `undefined`).
export const pick = <T extends object, K extends keyof T>(obj: T, keys: K[]): Pick<T, K> =>
  keys.reduce((result, key) => {
    if (key in obj) {
      result[key] = obj[key];
    }
    return result;
  }, {} as Pick<T, K>);

export const getDoneWithMonth = (done: {date?: string}) =>
  !done.date ? done : {
    ...done,
    month: done.date.substr(0, MONTH_LENGTH)
  };
