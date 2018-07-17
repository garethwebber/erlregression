import React, { Component } from 'react';
import './App.css';

class App extends Component {
  constructor (props) {
	  super(props)

	  this.state = {
		  points: [],
		  regression: "",
		  count: 0,
		  graph: null,
	  };
	  this.loadDummyData = this.loadDummyData.bind(this);
	  this.getRegression = this.getRegression.bind(this);
  }

  componentDidMount() {
	this.getRegression();
  }

  render() {
    const points = this.state.points;
    const count = this.state.count;
    const regression = this.state.regression;
    const graph = this.state.graph;
    return (
      <div class="App">
        <header class="App-header">
          <h1 className="App-title">Erlang Regression App</h1>
        </header>
	<div class="leftcolumn"><p>There are {count} points:</p>
	    {points.map((point, index) =>
		   <p key={index}> ({point.point.x}, {point.point.y})</p>
	    )}
	<p>{regression}</p>
	</div>
	<div class="rightcolumn">
	{count < 2 && 
	<span>
                <p>You need two or more points to run a regression. Enter some
                using form to the left, or load some dummy data</p>.
                <button onClick={this.loadDummyData}>Load Dummy Data</button>
        </span>
	}
        {count > 1 && <img src={graph}/>}
	</div>
	<div class="footer">
	<hr />
        <p>Erlang/React example application by Gareth Webber. Released under open source&nbsp; 
	   <a href="https://raw.githubusercontent.com/garethwebber/erlregression/master/LICENSE">licence</a>.<br />
	Swagger <a href="/api-docs">API documentation</a> for the
	    Elang provide REST interface.</p>
	</div>
	</div>
    );
  }

  async getRegression() {
    // Get Points from the database
    const req1 = await fetch('/rest/point', {
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
	console.log("Loaded " + this.state.count + " points");
    }).catch(err =>  {
        console.log("error: " + err);
    });

    // If there are more than two points run the regression
    if(this.state.count > 2) {
      let req2 = await fetch('/rest/regression', {
        method: 'GET',
        headers: {
            'Content-Type': 'application/json'
        }
      }).then(res => {
        return res.json();
      }).then(data =>{
        console.log("Regression result " + data);
      }).catch(err =>  {
        console.log("error: " + err);
      });

      // Get the graph and create a URL for it  
      let ret1 = fetch('/rest/graph', {
        method: 'GET',
        headers: {
//            'Content-Type': 'application/json'
        }
      }).then(res => {
	   return res.blob();
      }).then(graphblob => {
	let objectURL = URL.createObjectURL(graphblob);
	this.setState({ 
                graph: objectURL,
        });
	console.log("Got graph: " + objectURL + "bytes.");
      }).catch(err =>  {
        console.log("error: " + err);
      });
    }
    return true;
  }

  async loadDummyData() {
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
  let ret1 = await fetch('/rest/point', {
        method: 'PUT',
        body: dummy,
        headers: {
            'Content-Type': 'application/json'
        }
    }).then(res => {
	return res;
    }).catch(err =>  {
        console.log("error: " + err);
    });

    this.getRegression();
  }


}
export default App;
