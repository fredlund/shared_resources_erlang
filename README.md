# A FRAMEWORK FOR TESTING SHARED RESOURCES IMPLEMENTED IN JAVA AND ERLANG

This repository contains a framework, or toolbox, for testing a class of safety-critical concurrent systems implemented using shared resource specifications. Shared resources contain declarative specifications of process interaction that can be used to derive, in a model-driven way, the most delicate parts of a concurrent system. For further information about shared resources, see [this page](http://babel.ls.fi.upm.es/~rnnalborodo/sr_web/).
Our approach to testing shared resource system is based upon building a state-based model for the shared resource. The framework is implemented using Erlang, and the testing part of the framework uses the Quviq QuickCheck random testing tool.

### What is in this repository? ###

* The toolbox, which provides a generic (but inefficient) implementation of a shared resource specification, and a testing framework for systematically testing an implementation of a shared resource. 
* Two example shared resource specifications, a multibuffer and a robot warehouse

### Requirements ###

* [Erlang](http://www.erlang.org/)
* [JavaErlang](https://github.com/fredlund/JavaErlang)
* [Java](http://www.oracle.com/technetwork/es/java/javase/downloads/index.html)