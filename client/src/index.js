import React from 'react';
import ReactDOM from 'react-dom';
import PageFrame from './PageFrame';
import registerServiceWorker from './registerServiceWorker';
import { BrowserRouter } from 'react-router-dom';
import './index.css';

ReactDOM.render((
  <BrowserRouter>
    <PageFrame />
  </BrowserRouter>
), document.getElementById('root'));
registerServiceWorker();
