import {Response} from './models/Request';

export interface RequestProcessor {
  process(request, session): Promise<Response>;
}
