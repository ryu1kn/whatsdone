export type Done = {
  doneThing: string;
  date: string;
};

export type UserDone = {
  doneThing: string;
  date: string;
  userId: string;
};

export type DoneDiff = {
  doneThing?: string;
  date?: string;
};

export type DoneInDb = {
  id: string;
  date: string;
  doneThing: string;
  userId: string;
  month: string;
  topics?: string[];
};
