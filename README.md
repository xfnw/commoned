# commoned
ed but lisp, and with many weird opinionated changes

## building
a standalone `ce` executable may be built using [embeddable common
lisp](https://ecl.common-lisp.dev/) with the following command:
```sh
ecl --load build.cl --eval '(quit)'
```
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
- [ ] / - ce-command-search
- [ ] ? - ce-command-search-backwards
- [x] a - ce-command-add
- [x] A - ce-command-add-before
- [x] c - ce-command-line-replace
- [x] d - ce-command-delete
- [x] e - ce-command-open
- [x] h - ce-command-help
- [ ] i - ce-command-insert
- [ ] I - ce-command-insert-beg
- [ ] m - ce-command-move
- [x] p - ce-command-print
- [ ] s - ce-command-reg-replace
- [ ] t - ce-command-copy
- [x] w - ce-command-write
- [x] 0 - ce-command-number

