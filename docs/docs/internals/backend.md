---
layout: doc-page
title: "Backend Internals"
---

The code for the backend is split up by functionality and assembled in the
object `GenBCode`.

```
object GenBCode    --- [defines] -->        PlainClassBuilder       GenBCode also defines class BCodePhase, the compiler phase
     |                                              |
 [extends]                                      [extends]
     |                                              |
BCodeSyncAndTry    ---------------->        SyncAndTryBuilder
     |                                              |
BCodeBodyBuilder   ---------------->        PlainBodyBuilder
     |                                              |
BCodeSkelBuilder   ---------------->        PlainSkelBuilder
     |                                       /      |       \
 BCodeHelpers      ---------------->  BCClassGen BCAnnotGen ...  (more components)
     |      \       \
     |       \       \------------->  helper methods
     |        \       \------------>  JMirrorBuilder, JBeanInfoBuilder (uses some components, e.g. BCInnerClassGen)
     |         \
     |   BytecodeWriters  --------->        methods and classes to write byte code files
     |
 BCodeTypes        ---------------->        maps and fields for common BTypes, class Tracked, methods to collect information on classes, tests for BTypes (conforms), ...
     |
BCodeIdiomatic     ---------------->        utilities for code generation, e.g. genPrimitiveArithmetic
     |
  BCodeGlue        ---------------->        BType class, predefined BTypes
```

### Data Flow ###
Compiler creates a `BCodePhase`, calls `runOn(compilationUnits)`.

* initializes fields of `GenBCode` defined in `BCodeTypes` (BType maps,
  common BTypes like `StringReference`)
* initialize `primitives` map defined in `scalaPrimitives` (maps primitive
  members, like `int.+`, to bytecode instructions)
* creates `BytecodeWriter`, `JMirrorBuilder` and `JBeanInfoBuilder` instances
  (on each compiler run)
* `buildAndSendToDisk(units)`: uses work queues, see below.
  - `BCodePhase.addToQ1` adds class trees to `q1`
  - `Worker1.visit` creates ASM `ClassNodes`, adds to `q2`. It creates one
    `PlainClassBuilder` for each compilation unit.
  - `Worker2.addToQ3` adds byte arrays (one for each class) to `q3`
  - `BCodePhase.drainQ3` writes byte arrays to disk


### Architecture ###
The architecture of `GenBCode` is the same as in Scalac. It can be partitioned
into weakly coupled components (called "subsystems" below):


#### (a) The queue subsystem ####
Queues mediate between processors, queues don't know what each processor does.

The first queue contains AST trees for compilation units, the second queue
contains ASM ClassNodes, and finally the third queue contains byte arrays,
ready for serialization to disk.

Currently the queue subsystem is all sequential, but as can be seen in
http://magarciaepfl.github.io/scala/ the above design enables overlapping (a.1)
building of `ClassNodes`, (a.2) intra-method optimizations, and (a.3)
serialization to disk.

This subsystem is described in detail in `GenBCode.scala`

#### (b) Bytecode-level types, BType ####
The previous bytecode emitter goes to great lengths to reason about
bytecode-level types in terms of Symbols.

`GenBCode` uses `BType` as a more direct representation. A `BType` is immutable, and
a value class (once the rest of GenBCode is merged from
http://magarciaepfl.github.io/scala/ ).

Whether value class or not, its API is the same. That API doesn't reach into
the type checker. Instead, each method on a `BType` answers a question that can
be answered based on the `BType` itself. Sounds too simple to be good? It's a
good building block, that's what it is.

The internal representation of a `BType` is based on what the JVM uses: internal
names (e.g. `Ljava/lang/String;` ) and method descriptors; as defined in the JVM
spec (that's why they aren't documented in `GenBCode`, just read the [JVM 8 spec](https://docs.oracle.com/javase/specs/jvms/se8/html/)).

All things `BType` can be found in `BCodeGlue.scala`

#### (c) Utilities offering a more "high-level" API to bytecode emission ####
Bytecode can be emitted one opcode at a time, but there are recurring patterns
that call for a simpler API.

For example, when emitting a load-constant, a dedicated instruction exists for
emitting load-zero. Similarly, emitting a switch can be done according to one
of two strategies.

All these utilities are encapsulated in file `BCodeIdiomatic.scala`. They know
nothing about the type checker (because, just between us, they don't need to).

#### (d) Mapping between type-checker types and BTypes ####
So that (c) can remain oblivious to what AST trees contain, some bookkeepers
are needed:

  - Tracked: for a bytecode class (BType), its superclass, directly declared
    interfaces, and inner classes.

To understand how it's built, see:

```scala
final def exemplar(csym0: Symbol): Tracked = { ... }
```

Details in `BCodeTypes.scala`

#### (e) More "high-level" utilities for bytecode emission ####
In the spirit of `BCodeIdiomatic`, utilities are added in `BCodeHelpers` for
emitting:

- bean info class
- mirror class and their forwarders
- android-specific creator classes
- annotations


#### (f) Building an ASM ClassNode given an AST TypeDef ####
It's done by `PlainClassBuilder`(see `GenBCode.scala`).
