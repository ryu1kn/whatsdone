import React from 'react';
import {useSelector} from 'react-redux';
import {getColorCode} from '../../../util';
import {formatTime} from '../date-util';
import {Converter} from 'showdown';
import {RootState} from '../../../reducer';

const converter = new Converter();

interface DoneItemProps {
  doneId: string
  date: Date
  username: string
  children: string
  topics: string[]
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

const DoneThingInEdit = ({doneThing, updateDone}: { doneThing: string, updateDone: (done: string) => void }) => {
  const textareaRef = React.useRef<HTMLTextAreaElement>(null);
  return (
    <div className="doneitem__edit-container">
      <textarea
        className="form-control"
        defaultValue={doneThing}
        autoFocus={true}
        ref={textareaRef}
        rows={3}
        onKeyDown={e => {
          if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) {
            updateDone((e.target as HTMLTextAreaElement).value);
          }
        }}
      />
      <button
        className="btn btn-default doneform__button"
        onClick={() => textareaRef.current && updateDone(textareaRef.current.value)}
        type="button"
      >
        Save
      </button>
    </div>
  );
};

const DoneThingInView = ({doneThing}: { doneThing: string }) => {
  return (
    <div className={doneItemCss('done-thing')}
         dangerouslySetInnerHTML={{__html: converter.makeHtml(doneThing)}}/>
  );
};

export const DoneItem = (props: DoneItemProps) => {
  const deleteDone = () => props.deleteDone(props.doneId);
  const updateDone = (newDoneThing: string) => props.updateDone(props.doneId, newDoneThing);
  const doneThing = props.children.toString();
  const isTopicsDisabled = useSelector((state: RootState) => state.done.features.includes('no-topics'));

  return (
    <div className={doneItemCss()} onDoubleClick={() => props.startEditDone(props.doneId)}>
      <div className={doneItemCss('user')}>
        <div className={doneItemCss('user-icon')} style={{backgroundColor: getIconColor(props.username)}}>
          {getFirstLetter(props.username)}
        </div>
        <div className={doneItemCss('user-name')}>{props.username}</div>
      </div>
      <div className={doneItemCss('body')}>
        {props.editInProgress ?
          <DoneThingInEdit doneThing={doneThing} updateDone={updateDone}/> :
          <DoneThingInView doneThing={doneThing}/>}
        <p className={doneItemCss('time')}>
          {formatTime(props.date)}
          {!isTopicsDisabled && props.topics.length > 0 && (
            <span className={doneItemCss('topics')}>
              {props.topics.map((topic, index) => (
                <span key={index} className={doneItemCss('topic')}>
                  {topic}
                </span>
              ))}
            </span>
          )}
        </p>
      </div>
      <div className={doneItemCss('delete-action') + ' glyphicon glyphicon-trash'}
           onClick={createOnClickDelete(deleteDone)}/>
    </div>
  );
};
