import React, { Component } from 'react';
import './App.css';

class App extends Component {
  constructor (props) {
	  super(props)

	  this.state = {
		  points: [],
		  regression: "",
		  count: 0,
	  };
	  this.loadDummyData = this.loadDummyData.bind(this);
	  this.runRegression = this.runRegression.bind(this);
  }

  componentDidMount() {
	this.getPoints();
	console.log("Running getPoints");
  }

  render() {
    const points = this.state.points;
    const count = this.state.count;
    const regression = this.state.regression;
    return (
      <div className="App">
        <header className="App-header">
          <h1 className="App-title">Erlang Regression App</h1>
        </header>
	 <div className="points"><p>There are {count} points:</p>
	    {points.map((point, index) =>
		   <p key={index}> ({point.point.x}, {point.point.y})</p>
	    )}
	 </div>
	<p>{regression}</p>
        <p className="App-intro">
        Click here to see <a href="/rest/point">database debug content.</a><br/>
        Click here to see the <a href="/api-docs">API documentation</a>
        </p>
        <button onClick={this.loadDummyData}>Load Dummy Data</button>
	<button onClick={this.runRegression}>Run Regression</button>
      </div>
    );
  }


  getPoints() {
    return fetch('/rest/point', {
        method: 'GET',
        headers: {
            'Content-Type': 'application/json'
        }
    }).then(res => {
        return res.json();
    }).then(data =>{
        this.setState({
		points: data,
		count: Object.keys(data).length,
	});
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
  return fetch('/rest/point', {
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

  runRegression() {
    return fetch('/rest/point', {
        method: 'GET',
        headers: {
            'Content-Type': 'application/json'
        }
        }).then(res => {
        return res.json();
    }).then(data =>{
	console.log("Data: " + data);
        return data;
	//this.setState({
        //        regression: data,
        //});
    }).catch(err =>  {
        console.log("error: " + err);
    });
  }
}
export default App;
