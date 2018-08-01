import React from 'react';
import { Switch, Route } from 'react-router-dom'
import {About, Footer, Header} from './components';
import 'typeface-roboto';
import App from './App';
import './PageFrame.css';

export default class PageFrame extends React.Component {

  componentDidMount() {
    document.title = "Erlang Regression App";
  }

  render() {
    return (
      <div className="App">
      <Header>Erlang Regression App</Header>
      
      <Switch>
        <Route exact path='/' component={App} />  
        <Route exact path='/about' component={About} />
      </Switch>

      {/* Footer */}
      <hr />
      <Footer />
      {/* END Footer */}
      </div>
    );
  }
}
