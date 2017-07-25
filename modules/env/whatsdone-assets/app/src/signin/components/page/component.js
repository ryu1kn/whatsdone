
import './style.less';

import React from 'react';

class Signin extends React.Component {

  constructor(params) {
    super(params);

    this.state = {
      email: '',
      password: ''
    };
    this.handleChange = this.handleChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
  }

  handleSubmit(event) {
    event.preventDefault();
    this.props.onSubmit(this.state);
  }

  handleChange(event) {
    this.setState({[event.target.name]: event.target.value.trim()});
  }

  render() {
    return (
      <div className="container signin">
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
      </div>
    );
  }

}

export default Signin;
