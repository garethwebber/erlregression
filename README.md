# ErlRegression

This is a test of my ability to learn erlang. The plan is to build a simple web app that can pass in a series of points and get a linear regression performed.

The application is built with rebar3. To run, execute `rebar3 shell` which will grab egd as a dependency and compile everything. In the shell type:

```erlang
  application:start(regression_app).
  whereis(regression_app) ! {self(), "runregression"}.
```

# Journey

+ Done. Get a working erlang build system using a simple factorial app
+ Done. Read a file of points, one point per line, comma seperated to load
+ Done. Perform a regression and output the details 
4. Produce a PNG graph of the points and the line
5. Make it work with a JSON payload and response
6. Make it a web service
7. Build a one page app to send the points and receive the response
