PREFIX ?= /usr/local
BINDIR ?= ${PREFIX}/bin

all: ce

develop: ce.cl
	ecl --load ce.cl --eval '(ce-repl)'

ce: ce.cl ce.asd build.cl
	ecl --shell build.cl

ce-sbcl: ce.cl
	sbcl --no-sysinit --no-userinit --load ce.cl \
	--eval "(save-lisp-and-die \"ce-sbcl\" :compression t :executable t :toplevel #'ce-main)"

install: all
	install ce ${DESTDIR}${BINDIR}

uninstall:
	rm -f ${DESTDIR}${BINDIR}/ce

# warning: clean does not actually set up for a clean build,
# ecl annoyingly hides build artifacts in its secret cache folder
clean:
	rm -f ce ce.fasb ce-sbcl

