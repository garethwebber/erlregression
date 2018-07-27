import React from 'react';
import {ListItem, IconButton, ListItemText, 
	ListItemSecondaryAction, Paper} from '@material-ui/core';

const PaperListItem = ({x, y, secondaryAction}) =>
            <Paper>
            <ListItem>
            <ListItemText primary={'(' + x + ', ' + y + ')'} />
           <ListItemSecondaryAction>
                <IconButton aria-label="Delete">
            <img width="16" height="16" alt="delete"
                  id={x + ',' + y}
                  src="/static/trash-can.png"
                  onClick={secondaryAction}/>
                </IconButton>
              </ListItemSecondaryAction>
            </ListItem>
            </Paper>

export default PaperListItem;
