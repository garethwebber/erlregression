import React from 'react';
import {AppBar, Button, Grid, List, ListItem, 
	IconButton, ListItemText, ListItemSecondaryAction,
	Paper, TextField, Typography} from '@material-ui/core';
import 'typeface-roboto';
import './App.css';

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
        <AppBar position="static"> 
	   <Typography variant='display1' align='center' color="inherit" gutterBottom>
              Erlang Regression App            
           </Typography>
        </AppBar>
	<Grid container xs={12} spacing={24444}>
	<Grid item xs={6}>
          <Typography variant="title" gutterBottom>
	  There are {count} points:
	  </Typography>
         
	  <List>
          {points.map((point, index) => (
            <Paper>
            <ListItem key={index}>
	    <ListItemText primary={'(' + point.point.x + ', ' +
                                   point.point.y + ')'} />
           <ListItemSecondaryAction>
                <IconButton aria-label="Delete">
            <img width="16" height="16" alt="delete"
		  id={'[{"point" : { "x" : ' + point.point.x + ', "y" : ' + point.point.y + '}}]'}
		  src="/static/trash-can.png"
		  onClick={this.handleDelete}/>
                </IconButton>
              </ListItemSecondaryAction>
            </ListItem>
            </Paper>
          ))}
          </List>

          <p>
            {regression}
          </p>
<form onSubmit={this.handleSubmit}>
  <TextField name="x" label="X" value={this.state.x} onChange={this.handleChange} margin="normal"/>
  <TextField name="y" label="Y" yalue={this.state.y} onChange={this.handleChange} margin="normal"/>
  <Button variant="raised" color="primary" type="submit">Add Point</Button>
</form>
          </Grid>
	  <Grid item xs={6}>
          {count < 3
	&& (
<span>
  <Typography align='center' gutterBottom>
  You need three or more points to run a regression. <br />Enter some
     using form to the left, or load some dummy data.
 </Typography> 
  <Button variant="raised" color="primary" onClick={this.loadDummyData}>Load Dummy Data </Button>
</span>
	)
	}
          {count > 2 && <img src={graph} />}
	</Grid>
        <Grid item xs={12}>
          <hr />
	  <Typography align='center' gutterBottom>
          Erlang/React example application by Gareth Webber. 
	     Released under open source&nbsp; 
	    <a href="https://raw.githubusercontent.com/garethwebber/erlregression/master/LICENSE">licence</a>.
            <br />
	    Swagger <a href="/api-docs">API documentation</a> for the
	    Elang provide REST interface.
          </Typography> 
	  </Grid>
	  </Grid>
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
    this.getRegression();
  };
}
