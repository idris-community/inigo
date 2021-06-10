*********
Reference
*********

Commands
========

``build``
---------

This command is used to build your package:

.. code-block::

    $ inigo build
    1/1: Building MyApp (MyApp.idr)

You can pass the codegen you want to use:

.. code-block::

    $ inigo build racket
    1/1: Building MyApp (MyApp.idr)

.. note::

    This does not build dependencies, for that see build-deps_

.. _build-deps:

``build-deps``
--------------

This command build your package's dependencies

.. code-block::

    $ inigo build-deps
    Building deps...
    Compiling Base/Fmt/Inigo.ipkg
    Compiled Base/Fmt/Inigo.ipkg

.. note::

    This does not fetch dependencies, for that see fetch-deps_.

``clean``
---------

This deletes artifacts from idris2's build process (``.ttc`` and ``.ttm`` files)

To clean artifacts from dependencies use ``clean deps``

``exec``
--------

This compiles and runs your project. You can optionally specify a codegen like ``build``
and pass arguments to the executable after ``--``

.. code-block::

    $ inigo exec refc -- --my-option 10

.. _fetch-deps:

``fetch-deps``
--------------

