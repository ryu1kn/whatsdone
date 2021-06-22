const pad0 = (n: number) => n < 10 ? `0${n}` : String(n);

export const getLocalDateString = (d: Date) =>
  `${d.getFullYear()}-${pad0(d.getMonth() + 1)}-${pad0(d.getDate())}`;

const hourIn12 = (h: number) => h > 12 ? h - 12 : h;

export const formatTime = (date: Date) => {
  const hour = date.getHours();
  const mins = date.getMinutes();
  const ampm = hour < 12 ? 'am' : 'pm';

  return `${hourIn12(hour)}:${pad0(mins)} ${ampm}`;
}
