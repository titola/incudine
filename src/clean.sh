#!/bin/sh

rm -f *.o libincudine* cygincudine* cache.lisp incudine analysis/maybe-fftw-no-simd.lisp
find -name \*~ -print | xargs rm -f
