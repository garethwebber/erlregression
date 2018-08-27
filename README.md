# ErlRegression

This is a test of my ability to learn Erlang. It is a simple web-app that you 
can give a series of points and get a linear regression performed. It is 
built with a react app talking to an Erlang provided REST interface. The 
application is built with rebar3 which will grab all the dependencies and 
compile everything. 

## Playing

There are a three ways to play with the app: through the react front end, 
through swagger, or via the erlang shell (talking to the OTP gen_server 
business logic implementation). 

### React App
Start the application on the command line using `rebar3 shell`. You can then 
spin up a web-browser and point it at `http://localhost:8080`. Alternatively,
the project can be pushed into `https://www.heroku.com` and will spin up 
without any further work (as a single web dyno).

![alt text](https://raw.githubusercontent.com/garethwebber/erlregression/master/priv/v1_graph.png "Regression App")

### Swagger
Start the application on the command line using `rebar3 shell`. You can then spin up a web-browser and point it at `http://localhost:8080/api-docs`.

### Erlang Shell
To run, execute `rebar3 shell` which will start erlang and load the regression_app. To see something enter: 

```erlang
regression_server:load_file("points.dat").
regression_server:run_regression().

regression_server:load_point({point, -2, 2}).
regression_server:run_regression().

regression_server:debug().
```
## Architecture

The architecture is best described in three parts: a react client-side
application, a set of rest endpoints served using cowboy and an OTP
gen_server based erlang application. The component diagram below details
the assembly. 

The react app is built under /client. The deployable version lives under
/priv/static. The Erlang code all lives under /src.

![alt text](https://raw.githubusercontent.com/garethwebber/erlregression/master/priv/architecture-diagram.png "Architecture of App")
