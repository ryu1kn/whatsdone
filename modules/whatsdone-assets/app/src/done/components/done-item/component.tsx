import React from 'react';
import {getColorCode} from '../../../util';
import {formatTime} from '../date-util';
import {Converter} from 'showdown';

const converter = new Converter();

interface DoneItemProps {
  doneId: string
  date: Date
  username: string
  children: string
  editInProgress?: boolean
  deleteDone: (id: string) => void
  startEditDone: (doneId: string) => void
  updateDone: (doneId: string, doneThing: string) => void
}

const doneItemCss = (...components: string[]) => ['doneitem', ...components].join('__');

const getFirstLetter = (name: string) => (name || '?')[0]!.toUpperCase();

const getIconColor = (name: string) => name ? getColorCode(name) : '#DDDDDD';

const createOnClickDelete = (deleteDone: () => void) => (e: any) => {
  e.stopPropagation();
  deleteDone();
};

const DoneThingInEdit = ({doneThing, updateDone}: { doneThing: string, updateDone: (done: string) => void }) =>
  <input type="text" className="form-control" defaultValue={doneThing}
         onKeyPress={e => e.key === 'Enter' && updateDone((e.target as HTMLInputElement).value)}/>;

const DoneThingInView = ({doneThing}: { doneThing: string }) =>
  <div className={doneItemCss('done-thing')}
       dangerouslySetInnerHTML={{__html: converter.makeHtml(doneThing)}}/>;

export const DoneItem = (props: DoneItemProps) => {
  const deleteDone = () => props.deleteDone(props.doneId);
  const updateDone = (newDoneThing: string) => props.updateDone(props.doneId, newDoneThing);
  const doneThing = props.children.toString();
  return (
    <div className={doneItemCss()} onDoubleClick={() => props.startEditDone(props.doneId)}>
      <div className={doneItemCss('user')}>
        <div className={doneItemCss('user-icon')} style={{backgroundColor: getIconColor(props.username)}}>
          {getFirstLetter(props.username)}
        </div>
        <div className={doneItemCss('user-name')}>{props.username}</div>
      </div>
      <div>
        {props.editInProgress ?
          <DoneThingInEdit doneThing={doneThing} updateDone={updateDone}/> :
          <DoneThingInView doneThing={doneThing}/>}
        <p className={doneItemCss('time')}>
          {formatTime(props.date)}
        </p>
      </div>
      <div className={doneItemCss('delete-action') + ' glyphicon glyphicon-trash'}
           onClick={createOnClickDelete(deleteDone)}/>
    </div>
  );
};
