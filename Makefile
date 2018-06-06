.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean -noshell

MODS = fac facfile

all: compile run

compile: ${MODS:%=%.beam} 

run:
	${ERL} -s fac main 5

clean:
	rm -rf *.beam erl_crash.dump

.DEFAULT:
    @$(ECHO) "Unknown target $@, try:  make help"
