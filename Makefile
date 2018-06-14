.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean -pa egd -noshell 

MODS = regression_app regression_math regression_graph

all: compile run

compile: ${MODS:%=%.beam} 

# In shell (erl -pa egd) call:
#  application:start(regression_app).
#  whereis(regression_app) ! {self(), "runregression"}.
run:
	${ERL} -eval "regression_math:run_regression(),init:stop()"
	${ERL} -eval "regression_graph:create_graph(),init:stop()"
	ls *png

clean:
	rm -rf *.png *.beam erl_crash.dump

.DEFAULT:
    @$(ECHO) "Unknown target $@, try:  make help"
