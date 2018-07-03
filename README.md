# ErlRegression

This is a test of my ability to learn erlang. The plan is to build a simple web app that you can give a series of points and get a linear regression performed.

The application is built with rebar3. 

To compile, execute `rebar3 compile` in the shell which will grab all the dependencies and compile everything. 

## Playing

There are a three ways to play with the app: through the react front end, through swagger, or via the erlang shell (passing messages to the app). Of these the react from end is the least developed.

### React App
Start the application on the command line using `rebar3 shell`. You can then spin up a web-browser and point it at `http://localhost:8080`.

### Swagger
Start the application on the command line using `rebar3 shell`. You can then spin up a web-browser and point it at `http://localhost:8080/api-docs`.

![alt text](https://raw.githubusercontent.com/garethwebber/erlregression/master/priv/v1_graph.png "Regression Graph")

### Erlang Shell
To run, execute `rebar3 shell` which will start erlang and load the regression_app. To see something enter: 

```erlang
whereis(regression_app) ! {self(), "loadfile", "points.dat"}.
whereis(regression_app) ! {self(), "runregression"}.         

whereis(regression_app) ! {self(), "loadpoint", {point, -1, 20}}.
whereis(regression_app) ! {self(), "runregression"}.             

whereis(regression_app) ! {self(), "debug"}.        
```

## Journey

+ Done. Get a working erlang build system using a simple factorial app
+ Done. Read a file of points, one point per line, comma seperated to load
+ Done. Perform a regression and output the details 
+ Done. Produce a PNG graph of the points and the line
+ Done. Use ETS to store points in DB, taking initial load from file. 
+ Done. Create three new message types: Add single point to DB; add multiple points to DB and add points from file to DB (stopping initial load).  
+ Done. Get cowboy webserver running serving static content
+ Done. Create a webservice endpoint that runs regression and returns graph
+ Done. Create webservice endpoints that allows points to be added
10. Place some protection around running regession without enough points.
11. Create nice React front-end rather than swagger.
12. Improve graph with ticks and other labelling.
13. Build a one page app to send the points and receive the response

