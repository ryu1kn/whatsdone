import React from 'react';
import {getColorCode} from '../../../util';
import {formatTime} from '../date-util';
import {Converter} from 'showdown';

const converter = new Converter();

interface DoneItemProps {
  doneId: string
  date: Date
  username: string
  deleteDone: (id: string) => void
  children: string
}

const doneItemCss = (...components: string[]) => ['doneitem', ...components].join('__')

const getFirstLetter = (name: string) => (name || '?')[0]!.toUpperCase();

const getIconColor = (name: string) => name ? getColorCode(name) : '#DDDDDD';

const createOnClickDelete = (deleteDone: () => void) => (e: any) => {
  e.stopPropagation();
  deleteDone();
};

export const DoneItem = (props: DoneItemProps) => {
  const rawMarkup = converter.makeHtml(props.children.toString());
  const deleteDone = () => props.deleteDone(props.doneId);
  return (
    <div className={doneItemCss()}>
      <div className={doneItemCss('user')}>
        <div className={doneItemCss('user-icon')} style={{backgroundColor: getIconColor(props.username)}}>
          {getFirstLetter(props.username)}
        </div>
        <div className={doneItemCss('user-name')}>{props.username}</div>
      </div>
      <div>
        <div className={doneItemCss('done-thing')} dangerouslySetInnerHTML={{__html: rawMarkup}}/>
        <p className={doneItemCss('time')}>
          {formatTime(props.date)}
        </p>
      </div>
      <div className={doneItemCss('delete-action') + ' glyphicon glyphicon-trash'} onClick={createOnClickDelete(deleteDone)}/>
    </div>
  );
}
