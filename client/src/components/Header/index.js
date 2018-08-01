import React from 'react';
import { Link } from 'react-router-dom';
import {AppBar, IconButton, Menu, MenuItem, Toolbar, Typography} from '@material-ui/core';
import MenuIcon from '@material-ui/icons/Menu';

class Header extends React.Component {
  state = {
    menuElement: null
  };
  
  constructor(props) {
          super(props);
          this.handleMenu  = this.handleMenu.bind(this);
          this.handleClose = this.handleClose.bind(this);
  }

  render() {
    const menuElement = this.state.menuElement;
    const menuOpen = Boolean(this.state.menuElement);
    const {
         children
    } = this.props;

    return(
        <AppBar style={{ marginLeft:-12, marginRight: 20}} position="static">
          <Toolbar>
            <IconButton colour="inherit" aria-label="Menu" onClick={this.handleMenu}>
              <MenuIcon />
            </IconButton>
            <Menu
              id="menu-appbar"
              anchorEl={menuElement}
              anchorOrigin={{
                vertical: 'top',
                horizontal: 'left',
              }}
              transformOrigin={{
                vertical: 'top',
                horizontal: 'left',
              }}
              open={menuOpen}
              onClose={this.handleClose}
              >
                <MenuItem component={Link} to="/">Main page</MenuItem>
                <MenuItem component={Link} to="/about">About this app</MenuItem>
              </Menu>

            <Typography variant='title' color="inherit">
              {children} 
            </Typography>
          </Toolbar>
        </AppBar>
    );
  };

  handleMenu = event => {
    this.setState({ menuElement: event.currentTarget });
  };

  handleClose = () => {
    this.setState({ menuElement: null });
  };

};

export default Header;
