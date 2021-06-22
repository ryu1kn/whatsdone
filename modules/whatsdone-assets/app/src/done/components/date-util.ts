const pad0 = (n: number) => n < 10 ? `0${n}` : String(n);

export const getLocalDateString = (d: Date) =>
  `${d.getFullYear()}-${pad0(d.getMonth() + 1)}-${pad0(d.getDate())}`;

const ONEDAY_MS = 24 * 60 * 60 * 1000;

const getDayLabel = (date: Date) =>
  date.toLocaleDateString('en-GB', {
    weekday: 'short',
    year: 'numeric',
    month: 'long',
    day: 'numeric'
  })

export const getFriendlyDayLabel = (dateString: string) => {
  const now = new Date();
  const today = getLocalDateString(now);
  const yesterday = getLocalDateString(new Date(now.getTime() - ONEDAY_MS));
  switch (dateString) {
    case today:
      return 'Today';
    case yesterday:
      return 'Yesterday';
    default:
      return getDayLabel(new Date(dateString));
  }
}

const hourIn12 = (h: number) => h > 12 ? h - 12 : h;

export const formatTime = (date: Date) => {
  const hour = date.getHours();
  const mins = date.getMinutes();
  const ampm = hour < 12 ? 'am' : 'pm';

  return `${hourIn12(hour)}:${pad0(mins)} ${ampm}`;
}
