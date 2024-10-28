# commoned
ed but lisp, and with many weird opinionated changes

## building
a standalone `ce` executable may be built using [embeddable common
lisp](https://ecl.common-lisp.dev/) with the following command:
```sh
ecl --shell build.cl
```

you'll need ecl with development bindings (probably called ecl-dev).
on alpine, make sure you also have libffi-dev gmp-dev and gc-dev
installed, since the dependencies for alpine's ecl package seem
to be missing stuff needed to build binaries.

note that the result will be dynamically linked to ecl by default, the
`--disable-shared` configure flag when building ecl may change this

