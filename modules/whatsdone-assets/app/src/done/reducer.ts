import ActionType from './action-type';
import {AnyAction} from 'redux';

export type NextKey = string

export type RawDoneItem = {
  date: string
  doneThing: string
  id: string
  userId: string
  username: string
  topics: string[]
}

export type DoneItem = Omit<RawDoneItem, 'date'> & {
  date: Date
  editInProgress?: boolean
}

export type DoneState = {
  items: DoneItem[]
  nextKey: NextKey | null
  apiReady: boolean
  features: string[]
}

const initialState = {
  items: [],
  nextKey: null,
  apiReady: false,
  features: []
};

export default (state: DoneState = initialState, action: AnyAction) => {
  switch (action.type) {
  case 'API_READY':
    return Object.assign({}, state, {apiReady: true});
  case 'INIT_FEATURES':
    return {
      ...state,
      features: action.features || []
    };
  case ActionType.GET_DONE_REQUEST:
    return state;
  case ActionType.GET_DONE_SUCCESS:
    return {
      ...state,
      items: mergeDoneList(state.items, action.dones.map(normaliseDoneItem)),
      nextKey: action.nextKey
    };
  case ActionType.POST_DONE_REQUEST:
    return {
      ...state,
      items: [normaliseDoneItem(action.item), ...state.items],
      nextKey: state.nextKey
    };
  case ActionType.POST_DONE_SUCCESS:
    return {
      ...state,
      items: updateDones(state.items, normaliseDoneItem(action.item)),
      nextKey: state.nextKey
    };
  case ActionType.DELETE_DONE_REQUEST:
    return {
      ...state,
      items: state.items.filter(done => done.id !== action.id),
      nextKey: state.nextKey
    };
  case ActionType.START_EDIT_DONE:
    return {
      ...state,
      items: updateDoneById(state.items, action.id, done => ({...done, editInProgress: true}))
    };
  case ActionType.UPDATE_DONE_REQUEST:
    return {
      ...state,
      items: updateDoneById(state.items, action.id, done => ({
        ...done,
        doneThing: action.doneThing,
        editInProgress: false
      }))
    };
  case ActionType.GET_DONE_FAILURE:
  case ActionType.POST_DONE_FAILURE:
  case ActionType.DELETE_DONE_FAILURE:
  case ActionType.UPDATE_DONE_FAILURE:
    console.error(action.error.stack);
    /* fall through */
  default:
    return state;
  }
};

function mergeDoneList(items1: DoneItem[], items2: DoneItem[]) {
  return [...items1, ...items2].sort(
    (a, b) =>
      a.date < b.date ? 1 :
        a.date > b.date ? -1 : 0
  );
}

function normaliseDoneItem(doneItem: RawDoneItem): DoneItem {
  return Object.assign({}, doneItem, {date: new Date(doneItem.date)});
}

function updateDones(dones: DoneItem[], doneItem: DoneItem) {
  const index = dones.findIndex(done => done.date.getTime() === doneItem.date.getTime());
  if (index === -1) return dones;
  return [...dones.slice(0, index), doneItem, ...dones.slice(index + 1)];
}

function updateDoneById(dones: DoneItem[], id: string, convert: (i: DoneItem) => DoneItem) {
  const index = dones.findIndex(done => done.id === id);
  return [...dones.slice(0, index), convert(dones[index]!), ...dones.slice(index + 1)];
}
