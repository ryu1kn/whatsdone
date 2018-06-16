import {Request} from '../../lib/LambdaRequestNormaliser';

export const request: Request = {
  path: '/PATH',
  params: {id: 'DONE_ID'},
  query: {},
  body: {},
  userInfo: {
    username: 'USERNAME',
    userId: 'COGNITO_USER_ID'
  }
};

export const session = {
  userId: 'USER_ID',
  username: 'USER_NAME'
};
