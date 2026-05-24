
export const MONTH_LENGTH = 'YYYY-MM'.length;

export const getDoneWithMonth = (done: {date?: string}) =>
  !done.date ? done : {
    ...done,
    month: done.date.substr(0, MONTH_LENGTH)
  };
