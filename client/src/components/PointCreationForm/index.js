import React from 'react';
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
    return (
          <form onSubmit={this.handleSubmit}>
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

}
