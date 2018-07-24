import React from 'react';
import {Typography} from '@material-ui/core';

const HeadSubHead = ({heading, subheading}) =>
      <span>
          <Typography variant="title">
          {heading}
          </Typography>
          <Typography variant="subheading">
          {subheading}
          </Typography>
      </span>

export default HeadSubHead;
