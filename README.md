ccti
====

*ccti* (Curry Concolic Testing Interpreter) is a tool for automated generation
of test cases for the functional logic language
[Curry](http://www-ps.informatik.uni-kiel.de/currywiki/).

To learn more about Curry take a look at the
[Curry report](http://www-ps.informatik.uni-kiel.de/currywiki/_media/documentation/report.pdf).

# Installation

## Installation of z3

*ccti* makes use of the [*z3* SMT solver](https://github.com/Z3Prover/z3).
Since *ccti* uses version 2.6 of the
[SMT-LIB](http://smtlib.cs.uiowa.edu/index.shtml) standard which is not yet
supported in the latest release version of *z3* (4.5.0), you have to install
the latest version from the git repository.
You can specify the installation directory for *z3* using the `--prefix` option:

~~~
git clone https://github.com/Z3Prover/z3.git
cd z3
python scripts/mk_make.py --prefix=$HOME/z3/master
cd build
make
make install
~~~

After having installed *z3* please make sure that you add the *z3* binary to
your path.

## Installation of PAKCS (Version 2)

In order to install *ccti* you need the *PAKCS* Curry compiler (with type classes).
You can either download the development release
[here](https://www.informatik.uni-kiel.de/~pakcs/download.html) or you can build
*PAKCS* from source by checking out the latest version from gitlab using the
following command:

`git clone https://git.ps.informatik.uni-kiel.de/curry/pakcs.git`

For installation of *PAKCS* please refer to the instructions provided in the
pakcs folder (`INSTALL.txt` for the tarball and `GITINSTALL.txt` in case you
build from source).

For Ubuntu/Debian systems there are also DEB packages of *PAKCS*.
[Here](https://packages.ps.informatik.uni-kiel.de/curry/index.html) you can
find out how to install them.

## Installation of *ccti*

After *PAKCS* has been successfully installed, you can install *ccti*.
If you haven't already done this, first checkout the latest version from the
git repository:

`git clone https://git.informatik.uni-kiel.de/jrt/ccti.git`

In order to build *ccti* please make sure that the binary of the Curry Package
Manager is in your path, e.g. by executing `cypm --help`.
The Curry Package Manager is required to install some of the dependencies of
*ccti*. You can learn more about the package manager itself
[here](http://www-ps.informatik.uni-kiel.de/currywiki/tools/cpm).

To build *ccti* change into the corresponding directory, e.g. `cd $HOME/ccti`
and execute `make install`.
Afterwards you will find the binary of *ccti* in the `bin` folder.
In order to make it globally accessible you should add `$HOME/ccti/bin` to your
path.

# Removing *ccti*

To remove *ccti* simply remove the `ccti` folder, e.g. assuming you installed
it in your `$HOME` directory:

`rm -rf $HOME/ccti`

# Usage of *ccti*

In order to apply *ccti* for the automated generation of test cases for a Curry
function you need to call its binary with Curry module.
This Curry module has to include a `main` function.
The body of the `main` function has to be a call (with concrete input data)
to the function which shall be concolically tested.
This call is required to initialize the concolic testing interpreter.

For instance, take a look at the following Curry module:

~~~{.haskell}
data Nat = IHi
         | O Nat
         | I Nat
 deriving Eq

add :: Nat -> Nat -> Nat
add IHi   y     = succ y
add (O x) IHi   = I x
add (O x) (O y) = O (add x y)
add (O x) (I y) = I (add x y)
add (I x) IHi   = O (succ x)
add (I x) (O y) = I (add x y)
add (I x) (I y) = O (add (succ x) y)

succ :: Nat -> Nat
succ IHi   = O IHi
succ (O x) = I x
succ (I x) = O (succ x)

main :: Nat
main = add IHi (I (O IHi))
~~~

This module includes a representation of positive binary numbers in Curry
as well as an operation `add` to add two binaries.
In order to generate test cases for `add` we provide a `main` function
with the initial call `add IHi (I (O IHi))`.

Applying *ccti* on this module produces the following output including six
test cases for `add`:

~~~
examples> ccti Nat.curry
========================================================================================================================
Generating FlatCurry code
[1 of 2] Skipping  Prelude          ( /local/jrt/pakcs/lib/Prelude.curry, /local/jrt/pakcs/lib/.curry/Prelude.fcy )
[2 of 2] Compiling Nat              ( Nat.curry, .curry/Nat.fcy )
========================================================================================================================
Reading FlatCurry file(s)
========================================================================================================================
Annotating case expressions with fresh identifiers
========================================================================================================================
Generating SMT-LIB declarations for FlatCurry types
========================================================================================================================
Beginning with concolic search
(Nat.add (Nat.O Nat.IHi) Nat.IHi = {Nat.I Nat.IHi})
(Nat.add (Nat.I Nat.IHi) (Nat.O Nat.IHi) = {Nat.I (Nat.O Nat.IHi)})
(Nat.add (Nat.I Nat.IHi) Nat.IHi = {Nat.O (Nat.O Nat.IHi)})
(Nat.add (Nat.I Nat.IHi) (Nat.I (Nat.O Nat.IHi)) = {Nat.O
                                                   (Nat.O (Nat.O Nat.IHi))})
(Nat.add (Nat.O Nat.IHi) (Nat.I (Nat.O Nat.IHi)) = {Nat.I (Nat.I Nat.IHi)})
(Nat.add Nat.IHi (Nat.I (Nat.O Nat.IHi)) = {Nat.O (Nat.I Nat.IHi)})
~~~

You can find the `Nat` example as well as some other simple examples in the
`examples` folder of your *ccti* installation.

## Options for *ccti*

There are several options for *ccti*. You get an overview by executing
`ccti --help`.

For instance, you can change the coverage criterion used during concolic
execution. Per default branch coverage is used.
But there are examples in which this criterion is not sufficient, e.g.
`Perm.curry`. You can switch to function coverage by running the following
command:

`ccti --cover=function Perm.curry`

If you want to take a look at the SMT-LIB scripts generated by *ccti* and sent
to the *z3* SMT solver, you can produce a dump of these scripts by running
`examples> ccti --dump-smt Nat.curry`. The dump will be saved in a file in the
hidden directory `examples/.smt/`.
