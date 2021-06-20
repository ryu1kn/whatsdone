import ActionType from './action-type';

export type NextKey = string

export interface DoneState {
  items: any[]
  nextKey: NextKey
  apiReady: boolean
}

const initialState = {
  items: [],
  nextKey: null,
  apiReady: false
};

export default (state: DoneState = initialState, action) => {
  switch (action.type) {
  case 'API_READY':
    return Object.assign({}, state, {apiReady: true});
  case ActionType.GET_DONE_REQUEST:
    return state;
  case ActionType.GET_DONE_SUCCESS:
    return {
      items: mergeDoneList(state.items, action.dones.map(normaliseDoneItem)),
      nextKey: action.nextKey
    };
  case ActionType.POST_DONE_REQUEST:
    return {
      items: [normaliseDoneItem(action.item), ...state.items],
      nextKey: state.nextKey
    };
  case ActionType.POST_DONE_SUCCESS:
    return {
      items: updateDones(state.items, normaliseDoneItem(action.item)),
      nextKey: state.nextKey
    };
  case ActionType.DELETE_DONE_REQUEST:
    return {
      items: state.items.filter(done => done.id !== action.id),
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

function mergeDoneList(items1, items2) {
  return [...items1, ...items2].sort(
    (a, b) =>
      a.date < b.date ? 1 :
        a.date > b.date ? -1 : 0
  );
}

function normaliseDoneItem(doneItem) {
  return Object.assign({}, doneItem, {date: new Date(doneItem.date)});
}

function updateDones(dones, doneItem) {
  const index = dones.findIndex(done => done.date.getTime() === doneItem.date.getTime());
  if (index === -1) return dones;
  return [...dones.slice(0, index), doneItem, ...dones.slice(index + 1)];
}
