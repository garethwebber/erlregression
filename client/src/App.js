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
	<button onClick={loadDummyData}>Load Dummy Data</button>
      </div>
    );
  }
}

  function loadDummyData() {
  let dummy = `[
  {
    \"point\": {
      \"y\": -3,
      \"x\": -4
    }
  },
  {
    \"point\": {
      \"y\": 3,
      \"x\": -2
    }
  },
  {
    \"point\": {
      \"y\": 11,
      \"x\": -1
    }
  },
  {
    \"point\": {
      \"y\": 13,
      \"x\": 0
    }
  },
  {
    \"point\": {
      \"y\": 21,
      \"x\": 1
    }
  },
  {
    \"point\": {
      \"y\": 24,
      \"x\": 2
    }
  }
]`;
  console.log("Loading dummy data: " + dummy);
  return fetch('http://localhost:8080/rest/point', {
        method: 'PUT',
//        mode: 'CORS',
        body: dummy,
        headers: {
            'Content-Type': 'application/json'
        }
    }).then(res => {
	console.log("Returned");
        return res;
    }).catch(err =>  {
        console.log("error: " + err);
    });
  }

export default App;
