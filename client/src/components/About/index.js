import React from 'react';
import {Typography} from '@material-ui/core';

const About = () =>
      <span>
          <Typography variant="title" align="left" style={{ paddingTop: 24}}>
          About this application 
          </Typography>
          <Typography variant="body1" align="left" style={{ paddingTop: 12}}>
            This is a test of my ability to learn Erlang. It is a simple web-app that you can give a series of points
             and get a linear regression performed. It is built with a react app talking to an Erlang provided REST interface. 
          </Typography>
          <Typography variant="body1" align="left">
            The architecture is best described in three parts: a react client-side application, a set of rest endpoints served 
            using cowboy and an OTP gen_server based erlang application. The component diagram below details the assembly.
          </Typography>
          <Typography variant="body1" align="left">
            Have fun playing!
          </Typography>
          <Typography variant="body2" align="left" style={{ paddingTop: 12}}>
            Gareth Webber
          </Typography>
          <img class="reggraph" alt="Architecture Diagram" src="architecture-diagram.png"/>
      </span>

export default About;
