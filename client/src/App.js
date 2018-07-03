import React, { Component } from 'react';
import './App.css';

class App extends Component {
  constructor (props) {
	  super(props)

	  this.state = {
		  points: [],
	  };
	  this.loadDummyData = this.loadDummyData.bind(this);
  }

  componentDidMount() {
	this.getPoints();
	console.log("Running getPoints");
  }

  render() {
    const points = this.state.points;
    return (
      <div className="App">
        <header className="App-header">
          <h1 className="App-title">Erlang Regression App</h1>
        </header>
	 <div className="points">
	    {points.map((point, index) =>
		   <p key={index}> ({point.point.x}, {point.point.y})</p>
	    )}
	 </div>
        <p className="App-intro">
        Click here to see <a href="/rest/point">database debug content.</a><br/>
        Click here to see the <a href="/api-docs">API documentation</a>
        </p>
        <button onClick={this.loadDummyData}>Load Dummy Data</button>

      </div>
    );
  }


  getPoints() {
    return fetch('http://localhost:8080/rest/point', {
        method: 'GET',
        headers: {
            'Content-Type': 'application/json'
        }
    }).then(res => {
        return res.json();
    }).then(data =>{
        this.setState({points: data});
    }).catch(err =>  {
        console.log("error: " + err);
    });
  }

  loadDummyData() {
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
  }]`;
  return fetch('http://localhost:8080/rest/point', {
        method: 'PUT',
        body: dummy,
        headers: {
            'Content-Type': 'application/json'
        }
    }).then(res => {
	this.getPoints();
	return res;
    }).catch(err =>  {
        console.log("error: " + err);
    });
  }
}
export default App;
