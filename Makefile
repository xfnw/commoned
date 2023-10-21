PREFIX ?= /usr/local
BINDIR ?= ${PREFIX}/bin

all: ce

ce:
	ecl --load build.cl --eval '(quit)'

install: all
	install ce ${DESTDIR}${BINDIR}

uninstall:
	rm -f ${DESTDIR}${BINDIR}/ce

clean:
	rm -f ce ce.fasb

