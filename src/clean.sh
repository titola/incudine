#!/bin/sh

rm -f *.o *.so cache.lisp incudine analysis/maybe-fftw-no-simd.lisp
find -name \*~ -print | xargs rm -f
