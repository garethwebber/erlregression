import React from 'react';
import {Button, Typography} from '@material-ui/core';

const NoPointsText = ({buttonAction}) =>
  <span>
  <Typography align='center' gutterBottom>
    You need three or more points to run a regression. <br />Enter some
     using form to the left, or load some dummy data.
  </Typography>
  <Button variant="raised" color="primary" onClick={buttonAction}>Load Dummy Data </Button>
  </span>

export default NoPointsText;
