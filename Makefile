.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean -pa egd -noshell 

MODS = fac_app

all: compile run

compile: ${MODS:%=%.beam} 

run:
	${ERL} -eval "fac_app:run_regression()"
 	# In shell (erl -pa egd) call:  
        #  application:start(fac_app).
        #  whereis(regression) ! {self(), "runregression"}.

clean:
	rm -rf *.beam erl_crash.dump

.DEFAULT:
    @$(ECHO) "Unknown target $@, try:  make help"
