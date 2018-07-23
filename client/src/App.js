import React from 'react';
import {AppBar, Button, Grid, List, ListItem, 
	IconButton, ListItemText, ListItemSecondaryAction,
	Paper, TextField, Typography} from '@material-ui/core';
import 'typeface-roboto';
import './App.css';

const Header = ({children}) =>
	<AppBar position="static">
           <Typography variant='display1' align='center' color="inherit">
              {children} 
           </Typography>
        </AppBar>

const Footer = ({}) =>
          <Typography align='center'>
          Erlang/React example application by Gareth Webber.
             Released under open source&nbsp;
            <a href="https://raw.githubusercontent.com/garethwebber/erlregression/master/LICENSE">licence</a>.
            <br />
            Swagger <a href="/api-docs">API documentation</a> for the
            Elang provide REST interface.
          </Typography>
		
const HeadSubHead = ({heading, subheading}) =>
      <span>
          <Typography variant="title">
          {heading}
          </Typography>
          <Typography variant="subheading">
          {subheading}
          </Typography>
      </span>

const PaperListItem = ({x, y, secondaryAction}) =>
	    <Paper>
            <ListItem>
            <ListItemText primary={'(' + x + ', ' + y + ')'} />
           <ListItemSecondaryAction>
                <IconButton aria-label="Delete">
            <img width="16" height="16" alt="delete"
                  id={'[{"point" : { "x" : ' + x + ', "y" : ' + y + '}}]'}
                  src="/static/trash-can.png"
                  onClick={secondaryAction}/>
                </IconButton>
              </ListItemSecondaryAction>
            </ListItem>
            </Paper>

const NoPointsText = ({buttonAction}) =>
  <span>
  <Typography align='center' gutterBottom>
    You need three or more points to run a regression. <br />Enter some
     using form to the left, or load some dummy data.
  </Typography>
  <Button variant="raised" color="primary" onClick={buttonAction}>Load Dummy Data </Button>
  </span>

export default class App extends React.Component {
   state = {
                  points: [],
                  regression: '',
                  count: 0,
                  graph: null,
                  x: '',
                  y: '',        
  };

  constructor(props) {
	  super(props);
	  this.loadDummyData = this.loadDummyData.bind(this);
	  this.getRegression = this.getRegression.bind(this);
	  this.handleSubmit = this.handleSubmit.bind(this);
	  this.handleChange = this.handleChange.bind(this);
	  this.handleDelete = this.handleDelete.bind(this);
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
	<Header>Erlang Regression App</Header>
	    
	<Grid container xs={12} justify="space-around">
	
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
          <form onSubmit={this.handleSubmit}>
            <TextField name="x" label="X" value={this.state.x} onChange={this.handleChange} margin="normal"/>
            <TextField name="y" label="Y" yalue={this.state.y} onChange={this.handleChange} margin="normal"/>
            <Button variant="raised" color="primary" type="submit">Add Point</Button>
          </form>
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
    await fetch('/rest/point', {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
      },
    }).then(res => res.json()).then((data) => {
      newState.points = data;
      newState.count = Object.keys(data).length;
      console.log(`Loaded ${newState.count} points`);
    }).catch((err) => {
      console.log(`error fetching points: ${err}`);
    });

    // If there are more than two points run the regression
    if (newState.count > 2) {
      await fetch('/rest/regression', {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        },
      }).then(res => res.json()).then((data) => {
	const B = data.regression.B.toFixed(2);
	const A = data.regression.A.toFixed(2);
        
	console.log('Y = ' + B + 'x + ' + A + '.');
	newState.regression = 'Y = ' + B + 'x + ' + A + '.';
      }).catch((err) => {
        console.log(`error getting regression: ${err}`);
      });

      // Get the graph and create a URL for it
      await fetch('/rest/graph', {
        method: 'GET',
      }).then(res => res.blob()).then((graphblob) => {
        const objectURL = URL.createObjectURL(graphblob);
        newState.graph = objectURL;
        console.log(`Got graph: ${objectURL}bytes.`);
      }).catch((err) => {
        console.log(`error getting graph: ${err}`);
      });
    }

    this.setState(newState);
    return true;
  }

  handleChange(event) {
    const target = event.target.name;
    const value = event.target.value;

    this.setState({
      [target]: value
    });
  }

  async handleDelete(event) {
    const point = event.target.id;
 
    return await fetch('/rest/point', {
      method: 'DELETE',
      body: point,
      headers: {
        'Content-Type': 'application/json',
      },
    }).then(res => res).catch((err) => {
      console.log(`error: ${err}`);
    });
  }

  async handleSubmit(event) {
    const point = `[{
      "point": {
        "x": ` + this.state.x + `,
        "y": ` + this.state.y + `
      }
    }]`;
    await fetch('/rest/point', {
      method: 'PUT',
      body: point,
      headers: {
        'Content-Type': 'application/json',
      },
    }).then(res => res).catch((err) => {
      console.log(`error: ${err}`);
    });
    this.getRegression();
    return true;
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
    const ret1 = await fetch('/rest/point', {
      method: 'PUT',
      body: dummy,
      headers: {
        'Content-Type': 'application/json',
      },
    }).then(res => res).catch((err) => {
      console.log(`error: ${err}`);
    });
    this.getRegression();
  };
}
