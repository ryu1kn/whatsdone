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

const getFirstLetter = (name: string) => (name || '').charAt(0).toUpperCase() || '?';

const getIconColor = (name: string) => name ? getColorCode(name) : '#DDDDDD';

const createOnClickDelete = (deleteDone: () => void) => (e: any) => {
  e.stopPropagation();
  (e.target as Element).closest('.doneitem')?.setAttribute('style', 'display:none;');
  deleteDone();
};

export const DoneItem = (props: DoneItemProps) => {
  const rawMarkup = converter.makeHtml(props.children.toString());
  const deleteDone = () => props.deleteDone(props.doneId);
  return (
    <div className="doneitem">
      <div className="doneitem__user">
        <div className="doneitem__user-icon" style={{backgroundColor: getIconColor(props.username)}}>
          {getFirstLetter(props.username)}
        </div>
        <div className="doneiten__user-name">{props.username}</div>
      </div>
      <div>
        <div className="doneitem__done-thing" dangerouslySetInnerHTML={{__html: rawMarkup}}/>
        <p className="doneitem__time">
          {formatTime(props.date)}
        </p>
      </div>
      <div className="doneitem__delete-action glyphicon glyphicon-trash" onClick={createOnClickDelete(deleteDone)}/>
    </div>
  );
};
