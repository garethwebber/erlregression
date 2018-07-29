import React from 'react';
import {Grid, List} from '@material-ui/core';
import {Footer, Header, HeadSubHead, PaperListItem, 
	NoPointsText, PointCreationForm} from './components';
import axios from 'axios';
import 'typeface-roboto';
import './App.css';

export default class App extends React.Component {
   state = {
                  points: [],
                  regression: '',
                  count: 0,
                  graph: null,
  };

  constructor(props) {
	  super(props);
	  this.loadDummyData = this.loadDummyData.bind(this);
	  this.getRegression = this.getRegression.bind(this);
	  this.handleDelete = this.handleDelete.bind(this);
  }

  componentDidMount() {
    document.title = "Erlang Regression App";
    this.getRegression();
  }

  render() {
    const points = this.state.points;
    const count = this.state.count;
    const regression = this.state.regression;
    const graph = this.state.graph;
    return (
      <div className="App">
	<Header>Erlang Regression App</Header>
	    
	<Grid container xs={12} spacing={24} style={{ paddingTop: 24}}>
	
	{/* Left Hand Column */}
	<Grid item xs={6}>
	  <HeadSubHead
            heading={regression}
	    subheading={"There are " + count + " points."} />
	  <List>
          {points.map((point, index) => (
            <PaperListItem
		  x={point.point.x}
		  y={point.point.y}
		  secondaryAction={this.handleDelete} />
          ))}
          </List>
	  <PointCreationForm refreshAction={this.getRegression} />
          </Grid>
	  {/* END Left Hand Column */}
	  
	  {/* Right Hand Column */}
	  <Grid item xs={6}>
          {count < 3 
	     ? <NoPointsText buttonAction={this.loadDummyData} />
             : <img class="reggraph" alt="regresssion graph" src={graph} />}
	</Grid>
	{/* END Right Hand Column */}

	{/* Footer */}
        <Grid item xs={12}>
          <hr />
	  <Footer />
        </Grid>
	{/* END Footer */}
      
      </Grid>
      </div>
    );
  }

  async getRegression() {
    // Hold new state and update at end of method
   var newState = Object.assign({}, this.state);

    // Get Points from the database
    await axios('/rest/point')
      .then(res => {
      newState.points = res.data;
      newState.count = Object.keys(newState.points).length;
      console.log(`Loaded ${newState.count} points`);
    }).catch((err) => {
      console.log(`error fetching points: ${err}`);
    });

    // If there are more than two points run the regression
    if (newState.count > 2) {
      await axios('/rest/regression')
        .then(res => {
	const B = res.data.regression.B.toFixed(2);
	const A = res.data.regression.A.toFixed(2);
        
	console.log('Y = ' + B + 'x + ' + A + '.');
	newState.regression = 'Y = ' + B + 'x + ' + A + '.';
      }).catch((err) => {
        console.log(`error getting regression: ${err}`);
      });

      // Get the graph and create a URL for it
      await axios('/rest/graph', {responseType: 'blob'})
        .then(res => {
        const objectURL = URL.createObjectURL(res.data);
        newState.graph = objectURL;
        console.log(`Got graph: ${objectURL}bytes.`);
      }).catch((err) => {
        console.log(`error getting graph: ${err}`);
      });
    } else {
      newState.regression = "";
    }

    this.setState(newState);
    return true;
  }

  async handleDelete(event) {
    const url = '/rest/point/' + event.target.id;
    await axios.delete(url)
      .then(res => res).catch((err) => {
      console.log(`error: ${err}`);
    });
    this.getRegression();
  }

  async loadDummyData() {
    const dummy = `[
  {
    "point": {
      "y": -3,
      "x": -4
    }
  },
  {
    "point": {
      "y": 3,
      "x": -2
    }
  },
  {
    "point": {
      "y": 11,
      "x": -1
    }
  },
  {
    "point": {
      "y": 13,
      "x": 0
    }
  },
  {
    "point": {
      "y": 21,
      "x": 1
    }
  },
  {
    "point": {
      "y": 24,
      "x": 2
    }
  }]`;
    await axios.put('/rest/point', dummy)	  
      .then(res => res).catch((err) => {
      console.log(`error: ${err}`);
    });
    this.getRegression();
  };
}
