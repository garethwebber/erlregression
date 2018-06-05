.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean -noshell

MODS = fac

all: compile run

compile: ${MODS:%=%.beam} 

run:
	${ERL} -s fac main 5
	${ERL} -s fac main 10
	${ERL} -s fac main 15
	${ERL} -s fac main 20
	${ERL} -s fac main 25

clean:
	rm -rf *.beam erl_crash.dump

.DEFAULT:
    @$(ECHO) "Unknown target $@, try:  make help"
