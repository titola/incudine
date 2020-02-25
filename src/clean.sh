#!/bin/sh

rm -f *.o libincudine* cygincudine* barrier.h cache.lisp incudine analysis/maybe-fftw-no-simd.lisp
find -name \*~ -print | xargs rm -f
