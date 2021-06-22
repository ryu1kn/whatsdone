export const pad0 = (n: number) => n < 10 ? `0${n}` : String(n);

export const getLocalDateString = (d: Date) =>
  `${d.getFullYear()}-${pad0(d.getMonth() + 1)}-${pad0(d.getDate())}`;
