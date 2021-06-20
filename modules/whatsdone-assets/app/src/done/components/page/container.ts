import Action from '../../action';
import ServiceLocator from '../../../service-locator';
import {MapStateToPropsParam} from 'react-redux';
import {RootState} from '../../../reducer';
import {Dispatch} from 'redux';

interface ContainerState {
  ownProps?: any
}

const containerState: ContainerState = {};

const mapStateToProps: MapStateToPropsParam<RootState, ContainerState, RootState> = (state, ownProps) => {
  containerState.ownProps = ownProps;
  return {done: state.done};
};

const mapDispatchToProps = (dispatch: Dispatch) => {
  return {
    fetchDones: () => {
      dispatch(Action.getDones());
      ServiceLocator.whatsdoneApiClient.getDones()
        .then(response => {
          switch (response.status) {
          case 200: {
            const body = response.body;
            const dones = body.items || body;
            dispatch(Action.markGetDonesSuccess(dones, body.nextKey));
            return
          }
          case 401:
            ServiceLocator.configProvider.getConfig()
              .then(appConfig => {
                const signinUrl = buildSigninUrl(appConfig.CLIENT_ID);
                window.location.replace(signinUrl);
              });
            return
          default:
            throw new Error(`Unexpected response code ${response.status}`);
          }
        }).catch(e => {
          dispatch(Action.markGetDonesFailed(e));
        });
    }
  };
};

function buildSigninUrl(clientId: string) {
  const host = window.location.hostname;
  const hostname = host.split('.')[0];
  return `https://${hostname}.auth.ap-southeast-2.amazoncognito.com/login?response_type=token&client_id=${clientId}&redirect_uri=https://${host}`;
}

export {mapStateToProps, mapDispatchToProps};
