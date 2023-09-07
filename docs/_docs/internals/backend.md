---
layout: doc-page
title: "Backend Internals"
---

The code for the JVM backend is split up by functionality and assembled in
`GenBCode.scala`. This file defines class `GenBCode`, the compiler phase.

The workflow is split into `CodeGen.scala` Scala compilation context aware responsible for emitting bytecode,
and `PostProcessor.scala` which can be used for parallelized, context agnostic processing. In Scala 2 `PostProcessor`,
was responsible for performing bytecode optimization, e.g. inlining method calls. In Scala 3 it is only used for writing
Class files and Tasty to disk.

```
class CodeGen.Impl -[defines]-->        PlainClassBuilder
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
     |              \
     |               \------------->  helper methods
     |                \------------>  JMirrorBuilder, JAndroidBuilder (uses some components, e.g. BCInnerClassGen)
     |                 \----------->  `backendUtils`: utility for bytecode related ops, contains mapping for supported classfile version
     |
BCodeIdiomatic     ---------------->        utilities for code generation, e.g. genPrimitiveArithmetic
                    \-------------->        `bTypes`: maps and fields for common BTypes
                     \------------->        `int`: synchronized interface between PostProcessor and compiltion ctx
```

The `BTypes.scala` class contains the `BType` class and predefined BTypes

## Data Flow ##
Compiler creates a `GenBCode` `Phase`, calls `runOn(compilationUnits)`,
which calls `run(context)`. This:

* initializes lazily components reused by all `compilationUnits` using same instance of Context:
  - `bTypes`, used by `CodeGen` and `PostProcessro`, defined in `BCodeIdiomatic`  (BType maps, common BTypes like `StringRef`)
  - `backendInterface:` - proxy to Context specific operations
  - `codeGen: CodeGen` - uses `backendInterface`, `bTypes`, initializes instance of `DottyPrimitives` and defines `JMirrorBuilder` instance and implements bytecode generation flow (maps primitive members, like `int.+`, to bytecode instructions)
  - `fontendAccess` - synchronized `PostProcessor` interface to compiler settings, reporting and GenBCode context (e.g. list of entrypoints)
  - `postProcessor` - compilation context agnostic module dedicated to parallel processing of produced bytecode. Currently used only for writing Tasty and Class files. Defines `backendUtils` and `classfileWriter`
* sets context of current compilation unit to the shared context instance
* calls `codeGen.genUnit(ctx.compilation)` which returns structure with generated definitions (both Class files and Tasty)
* calls postProcessing of generated definition in `postProcessor`
* calls registered callbacks if needed for every generated class

Upon calling `codeGen.genUnit` it:
* creates `PlainClassBuilder` instance for each generated `TypeDef` and creates ASM `ClassNode`
* creates optional mirror class if needed
* generates Tasty file content and store its attributes in either mirror or plain class node

`PostProcessor` is later:
* enriching `ClassNode` with collected serializable lambdas
* sets its inner classes
* serializes class and writes it to file, optionally it can execute register callbacks for each generated file
* writes generated Tasty to file


## Architecture ##
The architecture of `GenBCode` is the same as in Scalac. It can be partitioned
into weakly coupled components (called "subsystems" below):

### (a) The queue subsystem ###
Queues mediate between processors, queues don't know what each processor does.

The first queue contains AST trees for compilation units, the second queue
contains ASM ClassNodes, and finally the third queue contains byte arrays,
ready for serialization to disk.

Currently the queue subsystem is all sequential, but as can be seen in
http://magarciaepfl.github.io/scala/ the above design enables overlapping (a.1)
building of `ClassNodes`, (a.2) intra-method optimizations, and (a.3)
serialization to disk.

This subsystem is described in detail in `GenBCode.scala`

### (b) Bytecode-level types, BType ###
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

### (c) Utilities offering a more "high-level" API to bytecode emission ###
Bytecode can be emitted one opcode at a time, but there are recurring patterns
that call for a simpler API.

For example, when emitting a load-constant, a dedicated instruction exists for
emitting load-zero. Similarly, emitting a switch can be done according to one
of two strategies.

All these utilities are encapsulated in file `BCodeIdiomatic.scala`. They know
nothing about the type checker (because, just between us, they don't need to).

### (d) Mapping between type-checker types and BTypes ###
So that (c) can remain oblivious to what AST trees contain, some bookkeepers
are needed:

  - Tracked: for a bytecode class (BType), its superclass, directly declared
    interfaces, and inner classes.

To understand how it's built, see:

```scala
final def exemplar(csym0: Symbol): Tracked = { ... }
```

Details in `BTypes.scala`

### (e) More "high-level" utilities for bytecode emission ###
In the spirit of `BCodeIdiomatic`, utilities are added in `BCodeHelpers` for
emitting:

- bean info class
- mirror class and their forwarders
- android-specific creator classes
- annotations


### (f) Building an ASM ClassNode given an AST TypeDef ###
It's done by `PlainClassBuilder`(see `CodeGen.scala`).
