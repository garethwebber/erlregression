import React from 'react';
import {AppBar, Typography} from '@material-ui/core';

const Header = ({children}) =>
        <AppBar position="static">
           <Typography variant='display1' align='center' color="inherit">
              {children} 
           </Typography>
        </AppBar>

export default Header;
