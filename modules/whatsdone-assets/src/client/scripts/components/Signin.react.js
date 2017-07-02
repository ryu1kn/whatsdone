
import React from 'react';
import LoginStatusAction from '../actions/LoginStatusAction';

class Signin extends React.Component {

  constructor(params) {
    super(params);

    this.handleSubmit = this.handleSubmit.bind(this);
  }

  handleSubmit(event) {
    event.preventDefault();
    LoginStatusAction.login({
      historyRef: this.props.history,
      formData: new FormData(event.target)
    });
  }

  render() {
    return (
      <div className="container signin">
        <form className="form-signin" onSubmit={this.handleSubmit}>
          <h2 className="form-signin-heading">Please sign in</h2>
          <label className="sr-only" htmlFor="inputEmail">Email address</label>
          <input className="form-control" type="email" id="inputEmail" name="email" placeholder="Email address" required autoFocus />
          <label className="sr-only" htmlFor="inputPassword">Password</label>
          <input className="form-control" type="password" id="inputPassword" name="password" placeholder="Password" required />
          <div className="checkbox">
            <label><input type="checkbox" name="rememberMe" />Remember me</label>
          </div>
          <button className="btn btn-lg btn-primary btn-block" type="submit">Sign in</button>
        </form>
      </div>
    );
  }

}

export default Signin;
