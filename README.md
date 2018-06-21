# ErlRegression

This is a test of my ability to learn erlang. The plan is to build a simple web app that you can give a series of points and get a linear regression performed.

The application is built with rebar3. 

To compile, execute `rebar3 compile` in the shell which will grab egd as a dependency and compile everything. 

To run, execute `rebar3 shell` which will start erlang and load the regression_app. To see something enter: 

```erlang
whereis(regression_app) ! {self(), "loadfile", "points.dat"}.
whereis(regression_app) ! {self(), "runregression"}.         

whereis(regression_app) ! {self(), "loadpoint", {point, -1, 20}}.
whereis(regression_app) ! {self(), "runregression"}.             

whereis(regression_app) ! {self(), "debug"}.        
```

# Journey

+ Done. Get a working erlang build system using a simple factorial app
+ Done. Read a file of points, one point per line, comma seperated to load
+ Done. Perform a regression and output the details 
+ Done. Produce a PNG graph of the points and the line
5. Use ETS to store points in DB, taking initial load from file. 
6. Create three new message types: Add single point to DB; add multiple points to DB and add points from file to DB (stopping initial load).  
7. Create a REST endpoint that runs regression and returns graph
8. Create REST endpoints that allows points to be added
9. Improve graph with ticks and other labelling.
10. Build a one page app to send the points and receive the response

# Output

The current regression graph looks like:

![alt text](https://raw.githubusercontent.com/garethwebber/erlregression/master/priv/v1_graph.png "Regression Graph")

