
import ActionType from './action-type';

const initialState = {
  items: [],
  nextKey: null
};

module.exports = (state = initialState, action) => {
  switch (action.type) {
  case ActionType.GET_DONE_REQUEST:
    return state;
  case ActionType.GET_DONE_SUCCESS:
    return {
      items: action.dones.map(normaliseDoneItem).sort(
        (a, b) =>
          a.date < b.date ? 1 :
          a.date > b.date ? -1 : 0
      ),
      nextKey: action.nextKey
    };
  case ActionType.POST_DONE_REQUEST:
    return {
      items: [normaliseDoneItem(action.item), ...state],
      nextKey: state.nextKey
    };
  case ActionType.POST_DONE_SUCCESS:
    return {
      items: updateDones(state, normaliseDoneItem(action.item)),
      nextKey: state.nextKey
    };
  case ActionType.DELETE_DONE_REQUEST:
    return {
      items: state.filter(done => done.id !== action.id),
      nextKey: state.nextKey
    };
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
