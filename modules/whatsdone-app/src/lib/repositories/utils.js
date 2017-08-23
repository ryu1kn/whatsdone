
const MONTH_LENGTH = 'YYYY-MM'.length;

exports.MONTH_LENGTH = MONTH_LENGTH;

exports.getDoneWithMonth = done => {
  if (!done.date) return done;
  return Object.assign({}, done, {
    month: done.date.substr(0, MONTH_LENGTH)
  });
};
