
import './style.less';

import React from 'react';
import LoginStatus from '../../login-status';

class Signin extends React.Component {

  constructor(params) {
    super(params);

    this.state = {
      email: '',
      password: '',
      newPassword: '',
      newPasswordRetype: ''
    };
    this.handleChange = this.handleChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
    this.handleNewPasswordSubmit = this.handleNewPasswordSubmit.bind(this);
  }

  handleSubmit(event) {
    event.preventDefault();
    this.props.onSubmit({
      email: this.state.email,
      password: this.state.password
    });
  }

  handleNewPasswordSubmit(event) {
    event.preventDefault();
    if (this.state.newPassword === this.state.newPasswordRetype) {
      this.props.onSubmitNewPassword(this.state.newPassword);
    }
  }

  handleChange(event) {
    this.setState({[event.target.name]: event.target.value.trim()});
  }

  render() {
    return (
      <div className="container signin">
        {this.props.status !== LoginStatus.NEW_PASSWORD_REQUIRED ? (
          <form className="form-signin" onSubmit={this.handleSubmit}>
            <h2 className="form-signin-heading">Please sign in</h2>
            <label className="sr-only" htmlFor="inputEmail">Email address</label>
            <input className="form-control" type="email" id="inputEmail" name="email" value={this.state.email} onChange={this.handleChange} placeholder="Email address" required autoFocus />
            <label className="sr-only" htmlFor="inputPassword">Password</label>
            <input className="form-control" type="password" id="inputPassword" name="password" value={this.state.password} onChange={this.handleChange} placeholder="Password" required />
            <div className="checkbox">
              <label><input type="checkbox" name="rememberMe" />Remember me</label>
            </div>
            <button className="btn btn-lg btn-primary btn-block" type="submit">Sign in</button>
          </form>
        ) : (
          <form className="form-signin" onSubmit={this.handleNewPasswordSubmit}>
            <h2 className="form-signin-heading">Please update password</h2>
            <label className="sr-only" htmlFor="inputNewPassword">New Password</label>
            <input className="form-control" type="password" id="inputNewPassword" name="newPassword" value={this.state.newPassword} onChange={this.handleChange} placeholder="New password" required autoFocus />
            <label className="sr-only" htmlFor="inputNewPasswordRetype">Retype New Password</label>
            <input className="form-control" type="password" id="inputNewPasswordRetype" name="newPasswordRetype" value={this.state.newPasswordRetype} onChange={this.handleChange} placeholder="Re-type new password" required />
            <button className="btn btn-lg btn-primary btn-block" type="submit">Update password</button>
          </form>
        )
      }
      </div>
    );
  }

}

export default Signin;
