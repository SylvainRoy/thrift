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

To push to github:
    > git push origin elisp


Tutorial:
=========

Ran the following:
    ../compiler/cpp/thrift -r -gen el tutorial.thrift

to test the output of the new "el" generator. (It comes in a gen-el directory.)


Run the elisp code:
===================

Either eval the EmacsLispClient.el file/buffer or:

    emacs -batch -l EmacsLispClient.el

(There must be a server from the tutorial running on 9090.)
