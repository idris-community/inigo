.. _tutorial:

***************
Getting started
***************

.. _install-source:

Installing from source
======================

Prerequisites
-------------

- `Idris2 <https://github.com/idris-lang/Idris2/>`_
- `Node <https://nodejs.org/>`_
- `Git <https://git-scm.com/>`_ (required for extra-deps)

.. code-block::

    $ git clone https://github.com/idris-community/inigo
    $ cd inigo
    $ make bootstrap install

This will build Inigo, Inigo's dependencies and install it to ``/usr/local/bin``.
You can specify where to install it by changing the value of ``INSTALL_DIR``
in ``config.mk``.

Installing a release
====================

Currently there are no releases, although this might change in the future.
So you have to install it from source.

First package
=============

.. note::

    *the central package repository* is a server hosted on CloudFlare's CDN and it's
    where some Inigo packages are hosted.

Now you've installed Inigo, it's time to make your first package.
First create a folder for you project, then use the ``inigo init`` command.

.. code-block::

    $ mkdir myapp
    $ cd myapp
    $ inigo init MyNamespace MyApp
    Initializing new inigo application MyNamespace.MyApp from template base skeleton with tests
    Successfully built BaseWithTest

You don't need to worry about ``MyNamespace`` and ``MyApp`` for now,
they are used when publishing packages to the central package repository.
This should create a few files:

.. code-block::

    Test
    | MyAppTest.idr
    | Suite.idr
    Inigo.toml
    MyApp.idr

Let's start with ``Inigo.toml``. This is the config file for your project,
and it contains all the dependencies and options.

.. code-block:: toml

    ns="MyNamespace"
    package="MyApp"
    version="0.0.1"

    description=""
    link=""
    readme=""
    modules=["MyApp"]
    depends=[]
    license=""
    main="MyApp"
    executable="MyApp"

    [deps]

    [dev-deps]
    Base.IdrTest="~0.0.1"

``ns`` stands for namespace and it's used if you publish to the central package repository.

``package`` is the name of your package.

``modules`` is a list of all the modules your package exports.

``depends`` is a list of the global dependencies (this should only be
packages bundled with idris2, such as contrib)

``deps`` describes your packages dependencies on the central package repository.
You specify the namespace and name of each package followed by any version
requirements, for instance:

.. code-block:: toml

    [deps]
    Base.Fmt = "~0.0.1"

``dev-deps`` is like ``deps`` but isn't required for releases.

Now you've got your package initialised, you can build it:

.. code-block::

    $ inigo build chez

Or run it:

-- code-block::

    $ inigo exec node
    1/1: Building MyApp (MyApp.idr)
    Executing MyApp with args []...
    Hello from MyNamespace.MyApp

``chez`` or ``node`` is the codegen to use while building.
You can omit it and Inigo will default to using node.
