---
layout: doc-page
title: "Scoped Caps"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/scoped-caps.html
---

## Introduction

When discussing [escape checking](basics.md#escape-checking), we referred to a scoping discipline.
That is, capture sets can contain only capabilities that are visible at the point where the set is
defined. But that raises the question: where is a universal capability `cap` defined? In fact, what
is written as the top type `cap` can mean different capabilities, depending on scope.

## Different Kinds of Caps

We will discuss three distinct kinds of `cap` in this chapter:

**Local caps**: Every class, method body, and block has its own local `cap`. It abstracts over the
capabilities used inside that scope, representing them by a single name to the outside world. Local
caps form a subcapturing hierarchy based on lexical nesting.

**Parameter caps**: When `cap` appears in a function parameter type (e.g., `def foo(x: T^)`), it
gets its own `cap` scoped to that parameter. At call sites, parameter caps are instantiated to the
actual capabilities passed in.

**Result caps**: When `cap` appears in a function result type (e.g., `def foo(x: T): U^`), it
becomes an existentially-bound `cap` that describes what the caller receives.

So, when writing `T^` (shorthand for `T^{cap}`), `cap` is a way of saying "captures something"
without naming what it is precisely, and depending of the context occurrence of such `cap`s, the
capture checker imposes restrictions on which capabilities are allowed to flow into them by means of
subcapturing. We will further expand on this idea (and other kinds of `cap`) later when discussing
[separation checking](separation-checking.md).

Another analogy for the different `cap`s is that they are some form of implicitly named existential
or abstract self-capture set attached to elements of the program structure, e.g., scopes,
parameters, or return values.

## Local Caps

Local `cap`s form a subcapturing hierarchy based on lexical nesting: a nested scope's local `cap`
subsumes its enclosing scope's local `cap`. This makes sense because the inner scope can use any
capability available in the outer scope as well as locally defined ones. At the top level, there is
a true universal `cap` — the local `cap` of the global scope — which all other local `cap`s
ultimately subsume:

```scala
// top level: the global `cap`
class Outer: // has local cap₁
  val f1: File^ = File("f1") // File^{cap₁}
  def method() = // has local cap₂
    val f2: File^ = File("f2") // File^{cap₂}
    var ref: () => Unit = null // () ->{cap₂} Unit, can accept what can flow into cap₂
    val closure = () => // has local cap₃
      val f3: File^ = File("f3") // File^{cap₃}
      val f4: File^ = f2 // ok, because {cap₂} <: {cap₃}
      val f5: File^ = f1 // ok, because {cap₁} <: {cap₃}
      ref = () => f3.read() // error, f3 is at the level of cap₃ and cannot flow into cap₂
      ...
```

Each capability has a _level_ corresponding to the local `cap` of its defining scope. The level
determines where a capability can flow: it can flow into `cap`s at the same level or more deeply
nested, but not outward to enclosing scopes (which would mean a capability lives longer than its
lexical lifetime). The compiler computes a capability's level by walking up the ownership chain
until reaching a symbol that represents a level boundary. Level boundaries are:
- **Classes** (but not inner non-static module classes)
- **Methods** (but not accessors or constructors)

Local values like `f1`, `f2`, `ref`, etc., don't define their own levels. They inherit the level of
their enclosing method or class. For example, this means:
- `f1` is at `Outer`'s level, i.e., `f1` subcaptures local `cap₁`.
- `f2` and `ref` are both at `method`'s level, i.e., both subcapture local `cap₂`.
- By lexical nesting, `{cap₂} <: {cap₃}` holds, but it does **not** hold that `{cap₃} <: {cap₂}`. Hence,
we cannot assign the closure to `ref`, because `{f3}` is subcapture-bounded by `{cap₃}`.

### Charging Captures

When a capability is used, it must be checked for compatibility with the capture-set constraints of
all enclosing scopes. This process is called _charging_ the capability to the environment.

```scala
def outer(fs: FileSystem^): Unit =
  def inner(): () ->{fs} Unit =
    () => fs.read()  // fs is used here
  inner()
```

When the capture checker sees `fs.read()`, it verifies that `fs` can flow into each enclosing scope:
1. The immediately enclosing closure `() => fs.read()` must permit `fs` in its capture set ✓
2. The enclosing method `inner` must account for `fs` (it does, via its capture set) ✓
3. The enclosing method `outer` must account for `fs` (it does, via its parameter) ✓

If any scope refuses to absorb the capability, capture checking fails:

```scala
def process(fs: FileSystem^): Unit =
  val f: () -> Unit = () => fs.read()  // Error: fs cannot flow into {}
```

The closure is declared pure (`() -> Unit`), meaning its local `cap` is the empty set. The
capability `fs` cannot flow into an empty set, so the checker rejects this.

### Visibility and Widening

When capabilities flow outward to enclosing scopes, they must remain visible. A local capability
cannot appear in a type outside its defining scope. In such cases, the capture set is _widened_ to
the smallest visible super capture set:

```scala
def test(fs: FileSystem^/*{cap₁*/}): Logger^/*{cap₂}*/ =
  val localLogger = Logger(fs)
  localLogger  // Type widens from Logger^{localLogger} to Logger^{fs}
```

Here, `localLogger` cannot appear in the result type because it's a local variable. The capture set
`{localLogger}` widens to `{fs}` (since `localLogger` captures `fs`) which widens to the parameter's
`cap₁`. In effect, `fs` flows via the parameter's `cap₁` into the result's `cap₂` (which is visible
outside of `test`), instead of `localLogger`.

#### Try-With-Resources, Again

Local `cap`s are one of the mechanisms that enable [escape checking](basics.md#escape-checking) for
the try-with-resources pattern. They prevent escaping of scoped capabilities through (direct or
indirect) assignment to mutable variables:
```scala
def withFile[T](block: File^ => T): T

var esc: File^/*{cap₁}*/ = null

withFile: f /* : File^{cap₂} */ =>
  esc = f   // error, since cap₂ cannot flow into cap₂
```

The other mechanism is careful treatment of `cap`s in function results
(cf. below) which prevents returning closures holding onto `f`.

Later sections on [capability classifiers](classifiers.md) will add a controlled mechanism that
permits capabilities to escape their level for situations where this would be desirable.

### Local Caps of Classes

A class receives its own local `cap` for the scope of its body. This `cap` serves as a template for
a new `cap` that will be attached to each instance of the class. Inside the class body, references
to the class's `cap` are implicitly prefixed by the path `this`:

```scala
class Logger(fs: FileSystem^): // local cap₁
  // Logger has its own local cap₁, accessed as this.cap₁
  val file: File^ = fs.open("log.txt")  // File^{this.cap₁}
  def log(msg: String): Unit = file.write(msg)
```

When a class inherits from other classes or traits, the `cap`s of all supertypes in the `extends`
clause are essentially unified with the `cap` of the current class. This unification happens because
all inherited members are accessed through `this`, and hence the local `cap`s will conform through
subtyping with each other:

```scala
trait Super: // local cap₁
  val doSomething: () => Unit // () ->{cap₁} Unit

class Logger(fs: FileSystem^) extends Super: // local cap₂
  val file: File^ = fs.open("log.txt") // File^{cap₂}
  def log(msg: String): Unit = file.write(msg)
  val doSomething = () => log("hello") // ok, since {file} <: {this.cap₂} =:= {this.cap₁}
```

As explained in [Capture Checking of Classes](classes.md), the capture checker infers and verifies
constraints on the contents of a class's `cap` through its self-type, reporting any inconsistencies.

When creating an instance, the class's template `cap` is substituted with a new `cap` specific to
the new object:

```scala
def test(fs: FileSystem^) = /* local cap₁ */
  val logger1 = Logger(fs)  // New logger1.cap for this instance, capturing fs
  val logger2 = Logger(fs)  // New logger2.cap, distinct from logger1.cap
```
Note that the `cap`s attached to `logger1` and `logger2` subsume the local `cap` of method `test` in accordance
to the rules outlined earlier.

Conceptually, a class' local `cap` behaves like an implicit [capture-set member](polymorphism.md#capability-members)
present in the class and all its supertypes:

```scala
class Logger(fs: FileSystem^) extends Super:
  type Cap^
  val file: File^{Cap} = ...
  // ...
```

## Parameter and Result Caps in Function Types

So far we've discussed local `cap`s that follow the lexical nesting hierarchy. But `cap` can also appear in function parameter and result types, where special binding rules apply.

### Existential Binding

Consider this method:

```scala
def makeLogger(fs: FileSystem^): Logger^ = new Logger(fs)
```

This creates a `Logger` that captures `fs`. We could have been more specific in specifying
`Logger^{fs}` as the return type, but the current definition is also valid, and might be preferable
if we want to hide details of what the returned logger captures. If we write it as above then
certainly the implied `cap` in the return type should be able to absorb the capability `fs`. This
means that this `cap` has to be defined in a scope in which `fs` is visible.

In logic, the usual way to achieve this scoping is with an existential binder. We can express the
type of `makeLogger` like this:
```scala
makeLogger: (fs: ∃cap₁.FileSystem^{cap₁}): ∃cap₂. Logger^{cap₂}
```
In words: `makeLogger` takes a parameter `fs` of type `Filesystem` capturing _some_ universal
capability `cap₁` and returns a `Logger` capturing some other (possibly different) universal `cap₂`.

We can also turn the existential in the function parameter to a universal "forall" in the function
itself. In that alternative notation, the type of `makeLogger` would read like this:
```scala
makeLogger: ∀cap₁.(fs: FileSystem^{cap₁}): ∃cap₂. Logger^{cap₂}
```
There's a connection with [capture polymorphism](polymorphism.md) here. `cap`s in function
parameters behave like additional capture parameters that can be instantiated at the call site to
arbitrary capabilities.

### Expansion Rules for Function Types

The conventions for method types carry over to function types. A dependent function type
```scala
(x: T) -> U^
```
is interpreted as having an existentially bound `cap` in the result, like this:
```scala
(x: T) -> ∃cap.U^{cap}
```
The same rules hold for the other kinds of function arrows, `=>`, `?->`, and `?=>`. So `cap` can in
this case absorb the function parameter `x` since `x` is locally bound in the function result.

However, the expansion of `cap` into an existentially bound variable only applies to functions that
use the dependent function style syntax, with explicitly named parameters. Parametric functions such
as `A => B^` or `(A₁, ..., Aₖ) -> B^` don't bind the `cap` in their return types in an existential
quantifier. For instance, the function
```scala
(x: A) -> B -> C^
```
is interpreted as
```scala
(x: A) -> ∃cap.B -> C^{cap}
```
In other words, existential quantifiers are only inserted in results of function arrows that follow
an explicitly named parameter list.

The placement of the existential reveals something about the behavior of a function of such a type.
For example, what flows into the `cap` of the result `C^{cap}` depends solely on `x` of type `A`
and is independent of the second
parameter of type `B`.

In effect, we have some form of "second-class" existential capture types which are carefully
restricted and implied by the type structure and `cap` occurrences. Unlike first-class existential
types, this approach does not require programmers to explicitly pack and unpack: the system
determines the binding structure automatically from where `cap` appears in the type.

#### Examples

 - `A => B` is an alias type that expands to `A ->{cap} B`, referring to scoped a `cap` in the context as outlined above.
 -  Therefore
   `(x: T) -> A => B` expands to `(x: T) -> ∃c.(A ->{c} B)`.

 - `(x: T) -> Iterator[A => B]` expands to `(x: T) -> ∃c.Iterator[A ->{c} B]`.

#### Summary

  - If a function result type follows a named parameter list and contains covariant occurrences of
    `cap`, we replace these occurrences with a new existential variable which is bound by a
    quantifier scoping over the result type.
  - If a function parameter type contains covariant occurrences of `cap`, we replace these
    occurrences with a new existential variable scoping over the parameter type.
  - Occurrences of `cap` elsewhere are not translated. They can be seen as representing an
    existential in the scope of the definition in which they appear.

### Parameter Caps and Local Caps

Inside the function body, parameter `cap`s are at the **same level** as the function's local `cap`.
This means the function's local `cap` can subsume capabilities from parameters:

```scala
def process(x: File^/* parameter {cap₁} */): Unit = /* local cap₂ */
  val y: File^/*{cap₂}*/ = x  // OK: x's cap is at process's level, same as process's local cap
  val f: () =>/*{cap₂}*/ Unit = () => x.read()  // OK: closure's local cap subsumes x
```

The parameter `x` has a capability at `process`'s level. The local `cap` of `process` (and any
nested closures) can subsume it because they're at the same level or more deeply nested.

### Result Caps are Isolated

Result `cap`s (i.e., those we assign an existential capture set in function-result types) cannot
absorb capabilities that would allow scoped resources to escape. Consider trying to leak a file by
directly returning a closure that captures it:

```scala
withFile[() => File^]("test.txt"): f =>
//       ^^^^^^^^^^^ T = () => File^, i.e., () ->{cap} File^{cap} for some outer cap
  () => f  // We want to return this as () => File^
```

The lambda `(f: File^) => () => f` has inferred type:
```scala
(f: File^) -> () ->{f} File^{f}
```
The inner closure explicitly captures `f`. To fit the expected type `File^ => () => File^`, we'd
need to widen through these steps:

```scala
(f: File^) -> () ->{f} File^{f}     // inferred: captures f explicitly
(f: File^) -> () ->{cap} File^{cap} // widen to cap
(f: File^) -> ∃c. () ->{c} File^{c} // apply existential rule for result cap
```

The final step produces a result cap `c` that is existentially bound to the outer function type. But
the expected type `() => File^` has a `cap` bound to the enclosing scope of `withFile` outside the
function entirely. The capture checker prevents the existentially bound `c` from flowing
into this outer `cap`, so the assignment fails.

Otherwise, allowing widening `∃c. () ->{c} File^{c}` to `() => File^` would let the scoped file escape:

```scala
val escaped: () => File^ = withFile[() => File^]("test.txt")(f => () => f)
//           ^^^^^^^^^^^ cap here is in the outer scope
escaped().read()  // Use-after-close!
```

By keeping result caps isolated, the capture checker ensures that an existential cannot hide `f` and
smuggle it out of its scope.

Beyond preventing escaping capabilities, the principle of isolating result `cap`s is important for
[tracking mutation and allocation effects](mutability.md) and
[separation checking](separation-checking.md), e.g., for a function returning a fresh new mutable
reference cell on each call
```scala
def freshCell(init: Int): Cell^ = new Cell(s)
val c1 = freshCell(0).set(42) // Cell^{cap₁}
val c2 = freshCell(11)        // Cell^{cap₂}, cap₁ and cap₂ are incomparable
```
a natural type would be `(init: Int) -> Cell^`, i.e., `(init: Int) -> ∃c.Cell[Int]^{c}`
by the rules above, reflecting that each
invocation returns a distinct new `Cell` instance.

## Comparison with Rust Lifetimes

Readers familiar with Rust may notice similarities to lifetime checking. Both systems prevent
references from escaping their valid scope. In Rust, a reference type `&'a T` carries an explicit
lifetime parameter `'a`. In Scala's capture checking, the lifetime is folded into the capability
name itself: `T^{x}` says "a `T` capturing `x`," and `x`'s level implicitly determines how long this
reference is valid. A capture set of a reference then acts as an upper bound of the reference
itself: it only lives as long as all the capabilities it contains are visible.

Here is how we could express the familiar `withFile` pattern in Rust:

```rust
struct File;
impl File { fn open(_path: &str) -> Option<File> { Some(File) } }

// Rust: the closure receives a reference bounded by 'a
fn with_file<R>(path: &str, f: impl for<'a> FnOnce(&'a File) -> R) -> R {
    let file = File::open(path).unwrap();
    f(&file)
}

fn main() {
    let f = File;
    let mut escaped: &File = &f;
    with_file("test.txt", |file| {
        escaped = file;  // Error: borrowed value does not live long enough
    });
}
```

In both Rust and Scala, the type system prevents the handle from escaping the callback. Rust
achieves this by requiring `'a` to be contained within the closure's scope. Scala achieves it by
checking that `file`'s level (tied to `withFile`) cannot flow into `escaped`'s level (at `main`).

The key analogies are:
- **Capability name ≈ Lifetime parameter**: Where Rust writes `&'a T`, Scala writes `T^{x}`. The
  capability `x` carries its lifetime implicitly via its level.
- **Capture set ≈ Lifetime bound**: A capture set `{x, y}` bounds the lifetime of a value to be no
  longer than the shortest-lived capability it contains.
- **Level containment ≈ Outlives**: Rust's `'a: 'b` (a outlives b) corresponds to Scala's level
check (outer scopes can flow into inner ones).

The key differences are:
- **What's tracked**: Rust tracks memory validity (preventing dangling pointers). Scala CC tracks
  capability usage (preventing unauthorized effects).
- **Explicit vs. implicit**: Rust lifetimes are explicit parameters (`&'a T`). Scala levels are
  computed automatically from program structure: you name the capability, not the lifetime.