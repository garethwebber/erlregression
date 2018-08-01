import React from 'react';
import {ListItem, IconButton, ListItemText, 
	      ListItemSecondaryAction, Paper} from '@material-ui/core';
import DeleteIcon from '@material-ui/icons/Delete';

const PaperListItem = ({x, y, secondaryAction}) =>
        <Paper>
          <ListItem>
            <ListItemText primary={'(' + x + ', ' + y + ')'} />
            <ListItemSecondaryAction>
                <IconButton aria-label="Delete" onClick={(e) => secondaryAction(e, (x + ',' + y))}>
                  <DeleteIcon />
                </IconButton>
            </ListItemSecondaryAction>
         </ListItem>
        </Paper>

export default PaperListItem;
