
![build](https://github.com/idris-community/inigo/actions/workflows/ci.yml/badge.svg)

# Inigo

Inigo is a package manager for Idris2 to help use and share Idris code.

Note: this project is in an **alpha test state**. Expect breaking changes at this point for the foreseeable future. Feel free to test and experiment and suggest changes.

## Installation

First, you'll need to install `inigo`. Note: we currently only support Idris2's built in code-gens on the client, but we plan to add support for external code-gens soon.

You can either download a release from [releases](https://github.com/hayesgm/inigo/releases) or bootstrap your own.

### Downloading a Release

We don't currently have any releases, but we may add them soon!

<!--
Download a release from the [releases](https://github.com/idris-community/inigo/releases) page. Install the file into your path and make sure it's executable.

```bash
curl -L https://github.com/idris-community/inigo/releases/download/0.0.1-alpha/inigo -o /usr/local/bin/inigo && chmod +x /usr/local/bin/inigo
```
-->

### Bootstrapping

#### Dependencies
- [idris2](https://github.com/idris-lang/Idris2)
- [idris2api](https://github.com/idris-lang/Idris2/blob/master/INSTALL.md#7-optional-installing-the-idris-2-api)

#### Building and installing

You can run the bootstrap Makefile command on Linux-type environments to bootstrap Inigo. Since Inigo depends on packages in Inigo, you'll need to bootstrap to get the components you need.

```bash
git clone https://github.com/hayesgm/inigo
cd inigo && make bootstrap
```

If all goes well, you'll want to make inigo available in your path, e.g. via:

```bash
cp build/exec/inigo /usr/local/bin
chmod +x /usr/local/bin/inigo
```

or simply:

```bash
make install
```

You can specify the install location by setting `INSTALL_DIR` in `config.mk` or on the command line

```bash
make install INSTALL_DIR="/usr/local/bin/
```

## Create an app

Let's create a new app and use Inigo to manage our packages:

``` bash
mkdir MyApp && cd MyApp
inigo init MyNamespace MyApp
```

You can read more about namespaces under the publishing a package section below.

This will create a skeleton app with an `Inigo.toml`. This configuration file will allow you to specify dependencies and automatically generates your Idris `ipkg` file for you.

You can build and run your app by calling:

```bash
inigo build
inigo exec
```

or test your app:

```bash
inigo fetch-deps prod --dev # pull dependencies
inigo build-deps
inigo test
```

You can specify the dependencies for your app in **Inigo.toml**:

```toml
...
[deps]
Base.Color="~0.0.1"
Base.Fmt="~0.0.1"
```

This allows us to use the [Color](https://inigo.pm/packages/Color) and [Fmt](https://inigo.pm/packages/Fmt) libraries. Then install the libaries and build the dependencies with:

```bash
inigo fetch-deps prod
inigo build-deps
```

Finally, you can use this in your app:

**MyApp.idr**

```idris
module MyApp

import Color
import Fmt

main : IO ()
main =
  putStrLn (fmt "Hello %s!" (decorate (Text Blue) "world"))
```

You can run this with:

```bash
inigo build && inigo exec
```

## Publishing a Package

To publish a package, you'll need to first register an account with Inigo to claim a namespace. A namespace is a name that will prefix your packages in the registry (i.e. to differentiate different packages with identical names).

```bash
inigo register
```

Follow the instructions, and then, you can login to your namespace:

```bash
inigo login
```

Finally, you can now publish your app:

```
inigo push ./Inigo.toml
```

## Questions

Feel free to ask questions on [GitHub issues](https://github.com/idris-community/inigo/issues).

## Contributing

Please feel free to open issues on GitHub or create pull requests. A key focus of this project will be to expand support for ChezScheme and other code-gens, as well as better support for Windows and other operating systems. Additionally, we would like to refactor much of the code to make better use of dependent types.
