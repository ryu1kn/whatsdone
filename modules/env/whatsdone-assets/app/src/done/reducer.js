
import ActionType from './action-type';

const initialState = [];

module.exports = (state = initialState, action) => {
  switch (action.type) {
  case ActionType.GET_DONE_REQUEST:
    return state;
  case ActionType.GET_DONE_SUCCESS:
    return action.dones.map(normaliseDoneItem).sort(
      (a, b) =>
        a.date < b.date ? 1 :
        a.date > b.date ? -1 : 0
    );
  case ActionType.POST_DONE_REQUEST:
    return [...state, normaliseDoneItem(action.item)];
  case ActionType.POST_DONE_SUCCESS:
    return updateDones(state, normaliseDoneItem(action.item));
  case ActionType.DELETE_DONE_REQUEST:
    return state.filter(done => done.id !== action.id);
  case ActionType.GET_DONE_FAILURE:
  case ActionType.POST_DONE_FAILURE:
  case ActionType.DELETE_DONE_FAILURE:
    console.error(action.error.stack);
    /* fall through */
  default:
    return state;
  }
};

function normaliseDoneItem(doneItem) {
  return Object.assign({}, doneItem, {date: new Date(doneItem.date)});
}

function updateDones(dones, doneItem) {
  const index = dones.findIndex(done => done.date.getTime() === doneItem.date.getTime());
  if (index === -1) return dones;
  return [...dones.slice(0, index), doneItem, ...dones.slice(index + 1)];
}
