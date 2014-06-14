Install:
========

I ran the 'apt-get install' from doc/install/debian.md

Then from root dir:
    ./bootstrap.sh
    ./configure
    make

Note that the only supported language in this setup was python.

I *didn't* do:
    make install


Test:
=====

Then, to test (watch out, this takes forever...):
    make -k check

Lots and lots of warning.
An error when testing with tornado 2.9. It's fixed when doing so with 3.2.1.

Then, to cross test:
    make cross

This one only works for python to python tests (so a very small subset).


Code:
=====

Created branch elisp.

New file:
t_el_generator.cc based on a copy of t_py_generator.

Changed file:
Makefile.am to register t_el_generator.cc for compilation.

New dir (and content):
lib/el to store the lib part (i.e. not generated) of the elisp implementation.


Tutorial:
=========

Ran the following:
    ../compiler/cpp/thrift -r -gen el tutorial.thrift

to test the output of the new "el" generator. (It comes in a gen-el directory.)


