import React, { Component } from 'react';
import './App.css';

class App extends Component {
  constructor(props) {
	  super(props);

	  this.state = {
		  points: [],
		  regression: '',
		  count: 0,
		  graph: null,
		  x: '',
		  y: '',
	  };
	  this.loadDummyData = this.loadDummyData.bind(this);
	  this.getRegression = this.getRegression.bind(this);
	  this.handleSubmit = this.handleSubmit.bind(this);
	  this.handleChange = this.handleChange.bind(this);
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
      <div className="App">
        <header className="App-header">
          <h1 className="App-title">Erlang Regression App</h1>
        </header>
        <div className="leftcolumn">
          <p>There are {count} points:
          </p>
          {points.map((point, index) => (
            <p key={index}>
	    (
              {point.point.x}
            ,
              {point.point.y}
            )
            </p>
          ))}
          <p>
            {regression}
          </p>
<form onSubmit={this.handleSubmit}>
  <label>
    X:
    <input type="text" name="x" value={this.state.x} onChange={this.handleChange}/>
  </label>
  <label>
    Y:
    <input type="text" name="y" yalue={this.state.y} onChange={this.handleChange}/>
  </label>
  <input type="submit" value="Add Point" />
</form>
	    </div>
        <div className="rightcolumn">
          {count < 3
	&& (
<span>
  <p>You need three or more points to run a regression. <br />Enter some
     using form to the left, or load some dummy data.
  </p>
  <button onClick={this.loadDummyData}>Load Dummy Data </button>
</span>
	)
	}
          {count > 2 && <img src={graph} />}
        </div>
        <div className="footer">
          <hr />
          <p>Erlang/React example application by Gareth Webber. 
	     Released under open source&nbsp; 
	    <a href="https://raw.githubusercontent.com/garethwebber/erlregression/master/LICENSE">licence</a>.
            <br />
	    Swagger <a href="/api-docs">API documentation</a> for the
	    Elang provide REST interface.
          </p>
        </div>
      </div>
    );
  }

  async getRegression() {
    // Get Points from the database
    const req1 = await fetch('/rest/point', {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
      },
    }).then(res => res.json()).then((data) => {
      this.setState({
        points: data,
        count: Object.keys(data).length,
      });
      console.log(`Loaded ${this.state.count} points`);
    }).catch((err) => {
      console.log(`error: ${err}`);
    });

    // If there are more than two points run the regression
    if (this.state.count > 2) {
      const req2 = await fetch('/rest/regression', {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        },
      }).then(res => res.json()).then((data) => {
        console.log(`Regression result ${data}`);
      }).catch((err) => {
        console.log(`error: ${err}`);
      });

      // Get the graph and create a URL for it
      const ret1 = fetch('/rest/graph', {
        method: 'GET',
      }).then(res => res.blob()).then((graphblob) => {
        const objectURL = URL.createObjectURL(graphblob);
        this.setState({
          graph: objectURL,
        });
        console.log(`Got graph: ${objectURL}bytes.`);
      }).catch((err) => {
        console.log(`error: ${err}`);
      });
    }
    return true;
  }

  handleChange(event) {
    const target = event.target.name;
    const value = event.target.value;

    this.setState({
      [target]: value
    });
  }

  async handleSubmit(event) {
    const point = `[{
      "point": {
        "x": ` + this.state.x + `,
        "y": ` + this.state.y + `
      }
    }]`;
    return await fetch('/rest/point', {
      method: 'PUT',
      body: point,
      headers: {
        'Content-Type': 'application/json',
      },
    }).then(res => res).catch((err) => {
      console.log(`error: ${err}`);
    });
  }

  async loadDummyData() {
    const dummy = `[
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
    const ret1 = await fetch('/rest/point', {
      method: 'PUT',
      body: dummy,
      headers: {
        'Content-Type': 'application/json',
      },
    }).then(res => res).catch((err) => {
      console.log(`error: ${err}`);
    });
  }
}
export default App;
