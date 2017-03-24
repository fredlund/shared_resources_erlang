# A FRAMEWORK FOR TESTING SHARED RESOURCES IMPLEMENTED IN JAVA AND ERLANG

This repository provides a toolbox for testing a class of safety-critical concurrent systems implemented using shared resource specifications. Shared resources contain declarative specifications of process interaction that can be used to derive, in a model-driven way, the most delicate parts of a concurrent system. For further information about shared resources, see [this page](http://babel.ls.fi.upm.es/~rnnalborodo/sr_web/).
Our approach to testing shared resource system is based upon building a state-based model for the shared resource. The framework is implemented using Erlang, and the testing part of the framework uses the Quviq QuickCheck random testing tool.

### What is in this repository? ###

* The toolbox, which provides a generic (but inefficient) implementation of a shared resource specification, and a testing framework for systematically testing an implementation of a shared resource. 
* A number of example shared resource specifications and implementations, including a multibuffer, a robot warehouse, a publish-and-subscribe system, and mergesort.

### Requirements ###

* [Erlang](http://www.erlang.org/)
* [QuickCheck](http://www.quviq.com/)

### Optional Requirements (for testing resources implemented in Java) ###

* [JavaErlang](https://github.com/fredlund/JavaErlang)
* [Java](http://www.oracle.com/technetwork/es/java/javase/downloads/index.html)

### Installation and Running ###

In the following it is assumed that Erlang, QuickCheck, Java and JavaErlang are
already installed. The Erlang-based shared resource toolbox has been developed
using Ubuntu (Linux). Installation instructions below assume running on Ubuntu
although with a bit more work the toolbox can be made to work on various other
platforms including other Linux distributions, macOS and even Windows.
Moreover, it is assumed that the current working directory is at the top
level of the toolbox, i.e., the directories src and ebin
(among others) are visible.

To compile the toolbox, including the examples, simply execute
```
#!bash
$ make
```

To access the functionalities of the toolbox start erlang with the appropriate libraries included for testing the mergesort resource implementations:
```
#!bash
$ erl -sname toolbox -pa ebin -pa examples/ebin/gritter -pa examples/mergesort/ebin
```

Once Erlang has started, we can run let QuickCheck run
a fixed set of tests provided in the file `mergesort_tests.erl`.
Basically all functions beginning with `prop_` and having zero
arguments -- providing QuickCheck properties -- will be checked:

```
#!erl
> eqc:module(mergesort_tests).

Starting Quviq QuickCheck version 1.39.2
   (compiled for R19 at {{2016,11,8},{14,38,41}})
   (Warning: You are using R18)
Licence for University UPM Madrid reserved until {{2017,3,24},{13,44,47}}
prop_gentest: Testing {mergesort_n_shr,false}; should succeed=false

*** Error: there are calls that have been completed by the implementation which cannot be completed by the model (when checking return values and without considering priority)
Final model state:
  {85,empty}
Schedule state:
  void
postcondition false after starting new jobs:
  << mergesorter:in(2,636) >>

Test failed with reason {postcondition,false}

Command sequence:
-----------------

  mergesorter:in(1,85) -- unblocks 1
  mergesorter:output() 
  mergesorter:in(1,934) 
  mergesorter:in(2,636) -- unblocks 2, output (returns 85) , 1


Failed! After 1 tests.
... (much more output)
```

We found a bug in an implementation of a mergesort shared resource. However,
since this was an expected bug testing will continue.


