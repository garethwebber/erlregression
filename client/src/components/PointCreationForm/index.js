import React from 'react';
import axios from 'axios';
import {Button, TextField} from '@material-ui/core';

export default class PointCreationForm extends React.Component {
   state = {
                  x: '',
                  y: '',        
  };

  constructor(props) {
	  super(props);
	  this.handleSubmit = this.handleSubmit.bind(this);
	  this.handleChange = this.handleChange.bind(this);
  }

  render() {
    const {
	 refreshAction
    } = this.props;

    return (
          <form onSubmit={e => this.handleSubmit(e, refreshAction)}>
            <TextField name="x" label="X" value={this.state.x} onChange={this.handleChange} margin="normal"/>
            <TextField name="y" label="Y" yalue={this.state.y} onChange={this.handleChange} margin="normal"/>
            <Button variant="raised" color="primary" type="submit">Add Point</Button>
          </form>
    );
  }



  handleChange(event) {
    const target = event.target.name;
    const value = event.target.value;

    this.setState({
      [target]: value
    });
  }

  async handleSubmit(event, refreshAction) {
    const point = `[{
      "point": {
        "x": ` + this.state.x + `,
        "y": ` + this.state.y + `
      }
    }]`;
    await axios.put('/rest/point', point) 
      .then(res => res).catch((err) => {
      console.log(`error: ${err}`);
    });
    refreshAction(); 
  }

}
