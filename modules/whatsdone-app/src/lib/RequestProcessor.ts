import {Response} from './models/Request';
import {Request} from './LambdaRequestNormaliser';
import {Session} from './LambdaRequestHandler';

export interface RequestProcessor {
  process(request: Request, session: Session): Promise<Response>;
}
