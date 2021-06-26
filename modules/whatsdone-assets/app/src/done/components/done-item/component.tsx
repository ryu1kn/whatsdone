import React from 'react';
import {getColorCode} from '../../../util';
import {formatTime} from '../date-util';
import {Converter} from 'showdown';
import {flow} from 'lodash';

const converter = new Converter();

interface DoneItemProps {
  doneId: string
  date: Date
  username: string
  deleteDone: (id: string) => void
  children: string
}

const DONE_CSS_NAMESPACE = 'doneitem';

const getFirstLetter = (name: string) => (name || '').charAt(0).toUpperCase() || '?';

const getIconColor = (name: string) => name ? getColorCode(name) : '#DDDDDD';

const grabTopElement = (el: Element) => el.closest(`.${DONE_CSS_NAMESPACE}`);
const hideElement = (el: Element) => el.setAttribute('style', 'display:none;');
const hideDone = flow(grabTopElement, hideElement)

const createOnClickDelete = (deleteDone: () => void) => (e: any) => {
  e.stopPropagation();
  hideDone(e.target as Element);
  deleteDone();
};

export const DoneItem = (props: DoneItemProps) => {
  const rawMarkup = converter.makeHtml(props.children.toString());
  const deleteDone = () => props.deleteDone(props.doneId);
  return (
    <div className={DONE_CSS_NAMESPACE}>
      <div className="doneitem__user">
        <div className="doneitem__user-icon" style={{backgroundColor: getIconColor(props.username)}}>
          {getFirstLetter(props.username)}
        </div>
        <div className="doneitem__user-name">{props.username}</div>
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
