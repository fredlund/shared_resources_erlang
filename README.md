# TESTING FRAMEWORK FOR SHARED RESOURCES JAVA IMPLEMENTATIONS

Framework for testing a class of safety-critical concurrent systems implemented using shared resource specifications. Shared resources contain declarative specifications of process interaction that can be used to derive, in a model-driven way, the most delicate parts of a concurrent system. For further information about shared resources, see [this page](http://babel.ls.fi.upm.es/~rnnalborodo/sr_web/).

The test is generating by building a state-based model that will help in testing a real Java implementation of the resource based on a given scheduling priority. The framework has been implemented using Erlang and QuickCheck and it source code is opensource. 

### What is in this repository? ###

* Framework Source Code
* Warehouse example with several sheduling policies
* Other examples are going to be upload...

### Requirements ###

* [Erlang](http://www.erlang.org/)
* [JavaErlang](https://github.com/fredlund/JavaErlang)
* [Java](http://www.oracle.com/technetwork/es/java/javase/downloads/index.html)
