
import React from 'react';
import LoginStatusAction from '../actions/LoginStatusAction';

class Signin extends React.Component {

  constructor(params) {
    super(params);
    console.log('initializing sigin page');

    this.state = {
      email: '',
      password: '',
      rememberMe: true
    };

    this.handleInputChange = this.handleInputChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
  }

  handleInputChange(event) {
    const target = event.target;
    const value = target.type === 'checkbox' ? target.checked : target.value;
    this.setState({[target.name]: value});
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
      <div className="container">
        <form className="form-signin" onSubmit={this.handleSubmit}>
          <h2 className="form-signin-heading">Please sign in</h2>
          <label className="sr-only" htmlFor="inputEmail">Email address</label>
          <input className="form-control" type="email" id="inputEmail" name="email" placeholder="Email address" value={this.state.email} onChange={this.handleInputChange} required autoFocus />
          <label className="sr-only" htmlFor="inputPassword">Password</label>
          <input className="form-control" type="password" id="inputPassword" name="password" placeholder="Password" value={this.state.password} onChange={this.handleInputChange} required />
          <div className="checkbox">
            <label><input type="checkbox" name="rememberMe" checked={this.state.isGoing} onChange={this.handleInputChange} />Remember me</label>
          </div>
          <button className="btn btn-lg btn-primary btn-block" type="submit">Sign in</button>
        </form>
      </div>
    );
  }

}

export default Signin;
