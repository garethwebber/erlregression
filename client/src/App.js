import React, { Component } from 'react';
import './App.css';

class App extends Component {
  render() {
    return (
      <div className="App">
        <header className="App-header">
          <h1 className="App-title">Erlang Regression App</h1>
        </header>
        <p className="App-intro">
        Click here to see <a href="/rest/point">database debug content.</a><br/>
	Click here to see the <a href="/api-docs">API documentation</a>
        </p>
      </div>
    );
  }
}

export default App;