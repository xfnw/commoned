# commoned
ed but lisp, and with many weird opinionated changes

## building
a standalone `ce` executable may be built using [embeddable common
lisp](https://ecl.common-lisp.dev/) with the following command:
```sh
ecl --load build.cl --eval '(quit)'
```

on alpine, attempting to build without ecl-dev and gc-dev installed will
result in strange and unintuitive error messages.

note that the result will be dynamically linked to ecl by default, the
`--disable-shared` configure flag when building ecl may change this

## are we editing yet?
implementation status of planned features:
- [x] q - quit
- [x] \n - ce-command-enter
- [x] : - ce-command-eval
- [x] ; - ce-command-eval-region
- [x] { - ce-command-expand-before
- [x] } - ce-command-expand
- [x] = - ce-command-get-point
- [x] , - ce-command-swap-point
- [x] / - ce-command-search
- [x] ? - ce-command-search-before
- [x] a - ce-command-add
- [x] A - ce-command-add-before
- [x] c - ce-command-line-replace
- [x] d - ce-command-delete
- [x] e - ce-command-open
- [x] h - ce-command-help
- [x] i - ce-command-insert
- [x] I - ce-command-insert-beg
- [ ] m - ce-command-move
- [x] n - ce-command-num-print
- [x] p - ce-command-print
- [x] s - ce-command-reg-replace
- [x] t - ce-command-copy
- [x] w - ce-command-write
- [x] x - ce-command-chop
- [x] X - ce-command-chop-beg
- [x] 0 - ce-command-number

