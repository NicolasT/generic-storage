generic-storage
===============

An implementation of "`Generic Storage in Haskell`_".

To run the demo::

    $ runhaskell -isrc demo.hs

Or, when using cabal-dev::

    $ cabal-dev install-deps
    $ runhaskell -package-conf=cabal-dev/packages-7.4.1.conf -isrc demo.hs

The output (at the time of writing) should be something like

::

    Rendering tree
    ("in",Leaf)
    ("in",Leaf)
    ("in",Leaf)
    ("in",Leaf)
    ("in",Branch "c" 3 () ())
    ("in",Branch "b" 2 () ())
    ("in",Branch "a" 1 () ())
    Looking up a node
    ("out",Branch "a" 1 () ())
    ("out",Leaf)
    ("out",Branch "b" 2 () ())
    ("out",Leaf)
    ("out",Branch "c" 3 () ())
    ("out",Leaf)
    ("out",Leaf)
    Result: Just 1

.. _Generic Storage in Haskell: http://www.andres-loeh.de/GenericStorage/wgp10-genstorage.pdf
