---
layout: default
title: "Dotty Overall Structure"
---

The compiler code is found in package [dotty.tools]. It spans the
following three sub-packages:

```none
backend     Compiler backends (currently for JVM and JS)
   dotc     The main compiler
     io     Helper modules for file access and classpath handling.
```

The [dotc] package contains some main classes that can be run as separate
programs. The most important one is class [Main]. `Main` inherits from
[Driver] which contains the highest level functions for starting a compiler
and processing some sources. `Driver` in turn is based on two other high-level
classes, [Compiler] and [Run].

Package Structure
-----------------
Most functionality of `dotc` is implemented in subpackages of `dotc`. Here's a
list of sub-packages and their focus.

```none
.
├── ast                 // Abstract syntax trees
├── config              // Compiler configuration, settings, platform specific definitions.
├── core                // Core data structures and operations, with specific subpackages for:
│   ├── classfile       // Reading of Java classfiles into core data structures
│   ├── tasty           // Reading and writing of TASTY files to/from core data structures
│   └── unpickleScala2  // Reading of Scala2 symbol information into core data structures
├── parsing             // Scanner and parser
├── printing            // Pretty-printing trees, types and other data
├── repl                // The interactive REPL
├── reporting           // Reporting of error messages, warnings and other info.
├── rewrite             // Helpers for rewriting Scala 2's constructs into dotty's.
├── transform           // Miniphases and helpers for tree transformations.
├── typer               //Type-checking and other frontend phases
└── util                // General purpose utility classes and modules.
```

Contexts
--------
`dotc` has almost no global state (the only significant bit of global state is
the name table, which is used to hash strings into unique names). Instead, all
essential bits of information that can vary over a compiler run are collected
in a [Context]. Most methods in `dotc` take a `Context` value as an implicit
parameter.

Contexts give a convenient way to customize values in some part of the
call-graph. To run, e.g. some compiler function `f` at a given phase `phase`,
we invoke `f` with an explicit context parameter, like this

```scala
f(/*normal args*/)(ctx.withPhase(phase))
```

This assumes that `f` is defined in the way most compiler functions are:

```scala
def f(/*normal parameters*/)(implicit ctx: Context) ...
```

Compiler code follows the convention that all implicit `Context` parameters are
named `ctx`.  This is important to avoid implicit ambiguities in the case where
nested methods contain each a Context parameters. The common name ensures then
that the implicit parameters properly shadow each other.

Sometimes we want to make sure that implicit contexts are not captured in
closures or other long-lived objects, be it because we want to enforce that
nested methods each get their own implicit context, or because we want to avoid
a space leak in the case where a closure can survive several compiler runs. A
typical case is a completer for a symbol representing an external class, which
produces the attributes of the symbol on demand, and which might never be
invoked. In that case we follow the convention that any context parameter is
explicit, not implicit, so we can track where it is used, and that it has a
name different from `ctx`. Commonly used is `ictx` for "initialization
context".

With these two conventions in place, it has turned out that implicit contexts
work amazingly well as a device for dependency injection and bulk
parameterization.  There is of course always the danger that an unexpected
implicit will be passed, but in practice this has not turned out to be much of
a problem.

Compiler Phases
---------------
Seen from a temporal perspective, the `dotc` compiler consists of a list of
phases. The current list of phases is specified in class [Compiler] as follows:

```scala
    def phases: List[List[Phase]] = List(
      List(new FrontEnd),           // Compiler frontend: scanner, parser, namer, typer
      List(new PostTyper),          // Additional checks and cleanups after type checking
      List(new Pickler),            // Generate TASTY info
      List(new FirstTransform,      // Some transformations to put trees into a canonical form
           new CheckReentrant),     // Internal use only: Check that compiled program has no data races involving global vars
      List(new RefChecks,           // Various checks mostly related to abstract members and overriding
           new CheckStatic,         // Check restrictions that apply to @static members
           new ElimRepeated,        // Rewrite vararg parameters and arguments
           new NormalizeFlags,      // Rewrite some definition flags
           new ExtensionMethods,    // Expand methods of value classes with extension methods
           new ExpandSAMs,          // Expand single abstract method closures to anonymous classes
           new TailRec,             // Rewrite tail recursion to loops
           new LiftTry,             // Put try expressions that might execute on non-empty stacks into their own methods
           new ClassOf),            // Expand `Predef.classOf` calls.
      List(new PatternMatcher,      // Compile pattern matches
           new ExplicitOuter,       // Add accessors to outer classes from nested ones.
           new ExplicitSelf,        // Make references to non-trivial self types explicit as casts
           new CrossCastAnd,        // Normalize selections involving intersection types.
           new Splitter),           // Expand selections involving union types into conditionals
      List(new VCInlineMethods,     // Inlines calls to value class methods
           new SeqLiterals,         // Express vararg arguments as arrays
           new InterceptedMethods,  // Special handling of `==`, `|=`, `getClass` methods
           new Getters,             // Replace non-private vals and vars with getter defs (fields are added later)
           new ElimByName,          // Expand by-name parameters and arguments
           new AugmentScala2Traits, // Expand traits defined in Scala 2.11 to simulate old-style rewritings
           new ResolveSuper),       // Implement super accessors and add forwarders to trait methods
      List(new Erasure),            // Rewrite types to JVM model, erasing all type parameters, abstract types and refinements.
      List(new ElimErasedValueType, // Expand erased value types to their underlying implementation types
           new VCElideAllocations,  // Peep-hole optimization to eliminate unnecessary value class allocations
           new Mixin,               // Expand trait fields and trait initializers
           new LazyVals,            // Expand lazy vals
           new Memoize,             // Add private fields to getters and setters
           new LinkScala2ImplClasses, // Forward calls to the implementation classes of traits defined by Scala 2.11
           new NonLocalReturns,     // Expand non-local returns
           new CapturedVars,        // Represent vars captured by closures as heap objects
           new Constructors,        // Collect initialization code in primary constructors
                                       // Note: constructors changes decls in transformTemplate, no InfoTransformers should be added after it
           new FunctionalInterfaces,// Rewrites closures to implement @specialized types of Functions.
           new GetClass),           // Rewrites getClass calls on primitive types.
      List(new LambdaLift,          // Lifts out nested functions to class scope, storing free variables in environments
                                       // Note: in this mini-phase block scopes are incorrect. No phases that rely on scopes should be here
           new ElimStaticThis,      // Replace `this` references to static objects by global identifiers
           new Flatten,             // Lift all inner classes to package scope
           new RestoreScopes),      // Repair scopes rendered invalid by moving definitions in prior phases of the group
      List(new ExpandPrivate,       // Widen private definitions accessed from nested classes
           new CollectEntryPoints,  // Find classes with main methods
           new LabelDefs),          // Converts calls to labels to jumps
      List(new GenSJSIR),           // Generate .js code
      List(new GenBCode)            // Generate JVM bytecode
    )
```

Note that phases are grouped, so the `phases` method is of type
`List[List[Phase]]`. The idea is that all phases in a group are *fused* into a
single tree traversal. That way, phases can be kept small (most phases perform
a single function) without requiring an excessive number of tree traversals
(which are costly, because they have generally bad cache locality).

Phases fall into four categories:

* Frontend phases: `Frontend`, `PostTyper` and `Pickler`. `FrontEnd` parses the
  source programs and generates untyped abstract syntax trees, which are then
  typechecked and transformed into typed abstract syntax trees.  `PostTyper`
  performs checks and cleanups that require a fully typed program. In
  particular, it

    - creates super accessors representing `super` calls in traits
    - creates implementations of synthetic (compiler-implemented) methods
    - avoids storing parameters passed unchanged from subclass to superclass in
      duplicate fields.

  Finally `Pickler` serializes the typed syntax trees produced by the frontend
  as TASTY data structures.

* High-level transformations: All phases from `FirstTransform` to `Erasure`.
  Most of these phases transform syntax trees, expanding high-level constructs
  to more primitive ones. The last phase in the group, `Erasure` translates all
  types into types supported directly by the JVM. To do this, it performs
  another type checking pass, but using the rules of the JVM's type system
  instead of Scala's.

* Low-level transformations: All phases from `ElimErasedValueType` to
  `LabelDefs`. These further transform trees until they are essentially a
  structured version of Java bytecode.

* Code generators: These map the transformed trees to Java classfiles or
  Javascript files.

[dotty.tools]: https://github.com/lampepfl/dotty/tree/master/src/dotty/tools
[dotc]: https://github.com/lampepfl/dotty/tree/master/src/dotty/tools/dotc
[Main]: https://github.com/lampepfl/dotty/blob/master/src/dotty/tools/dotc/Main.scala
[Driver]: https://github.com/lampepfl/dotty/blob/master/src/dotty/tools/dotc/Driver.scala
[Compiler]: https://github.com/lampepfl/dotty/blob/master/src/dotty/tools/dotc/Compiler.scala
[Run]: https://github.com/lampepfl/dotty/blob/master/src/dotty/tools/dotc/Run.scala
[Context]: https://github.com/lampepfl/dotty/blob/master/src/dotty/tools/dotc/core/Contexts.scala
