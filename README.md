<img src="http://www.polyml.org/images/Poly_Parrot3.gif" alt="Poly/ML logo" height="100" >

# Poly/ML

Poly/ML is a Standard ML implementation originally written in an experimental
language called [Poly](http://www.polyml.org/Doc.html#poly). It has been fully compatible with the [ML97 standard](http://sml-family.org/sml97-defn.pdf)
since version 4.0. For a full history, see [here](http://www.polyml.org/FAQ.html#history).

Poly/ML takes a conservative approach to the Standard ML language and avoids incompatible extensions.  It has added various library extensions particularly the thread library. Poly/ML's active development and unique features make it an exceptional implementation.

## Table of Contents

* [Features](#features)
* [Basis](#basis)
* [Installation](#installation)
* [Mailing List](#mailing-list)

## Features

* Fast compiler
* Preferred implementation for large projects such as [Isabelle](https://isabelle.in.tum.de/) and [HOL](https://hol-theorem-prover.org/).
* [Foreign function interface](http://www.polyml.org/documentation/Tutorials/CInterface.html) - allows static and dynamic libraries to be loaded in Poly/ML and exposes their functions as Poly/ML functions. See [here](https://www.mail-archive.com/polyml@inf.ed.ac.uk/msg00940.html) for an example of static linking.
* [Symbolic debugger](http://www.polyml.org/documentation/Tutorials/Debugging.html)
* [Windows programming interface](http://www.polyml.org/documentation/Tutorials/WindowsProgramming.html)
* [Thread library](http://www.polyml.org/documentation/Reference/Thread.xml) - provides a simplified version of Posix threads modified for Standard ML and allows Poly/ML programs to make use of multiple cores.  The garbage collector is also parallelised.

## Basis

The documentation for the Poly/ML Basis library can be found [here](http://www.polyml.org/documentation/Reference/Basis.html)
and includes information on global values and types as well as structures,
signatures and functors. More in-depth documentation can be found at
the SML Family website [here](http://sml-family.org/Basis/manpages.html).

## Installation

Poly/ML has native support for i386 (32- and 64-bit) and ARM (64-bit only).  It will also work on other architectures using a byte-code interpreter.  For more information, see the [download](http://www.polyml.org/download.html) page.

### Mailing List

There is a [mailing list](https://lists.polyml.org/postorius/lists/polyml.lists.polyml.org/) for Poly/ML for questions and support.
