
export const MONTH_LENGTH = 'YYYY-MM'.length;

export function getDoneWithMonth (done: {date?: string}) {
  if (!done.date) return done;
  return Object.assign({}, done, {
    month: done.date.substr(0, MONTH_LENGTH)
  });
}
