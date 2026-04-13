---
layout: doc-page
title: "Scoped Capabilities"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/scoped-capabilities.html
---

## Introduction

When discussing [escape checking](basics.md#escape-checking), we referred to a scoping discipline.
That is, capture sets can contain only capabilities that are visible at the point where the set is
defined. But that raises the question: where is a universal capability `any` defined?

The key principle is that each occurrence of `any` represents a _different_ capability. Each `any`
can subsume capabilities of the same or outer level, including other `any`s. This property is
central to [separation checking](separation-checking.md), where each `any` gets its own hidden set
of capabilities it subsumes.

Besides `any`, there is a related capability `fresh` (available via `import caps.fresh`) that can
appear in function type results. While `any` in a function result refers to the enclosing scope's
local `any`, `fresh` introduces a new existentially bound capability, guaranteeing that each call
produces a distinct, isolated result. We cover `fresh` in detail
[below](#parameter-and-result-anys-in-function-types).

## Where `any` Appears

We distinguish four positions where `any` (or `fresh`) can appear:

1. **Local `any`s**: Every class, method body, and block has its own local `any`. It abstracts over the
capabilities used inside that scope, representing them by a single name to the outside world. Local
`any`s form a subcapturing hierarchy based on lexical nesting.

2. **Parameter `any`s**: When `any` appears in a function parameter type (e.g., `def foo(x: T^)`), it
gets its own `any` scoped to that parameter. At call sites, parameter `any`s are instantiated to the
actual capabilities passed in.

3. **Result `any`s**: When `any` appears in a function result type (e.g., `A^ -> B^`, i.e.,
`A^{any} -> B^{any}`), it refers to the local `any` of the enclosing scope. Two calls to such a
function produce results with the same capture-set bound.

4. **Result `fresh`s**: Instead of `any`, one can write `fresh` in the result type of a function type
(e.g., `A^ -> B^{fresh}`). This indicates that each call to the function yields a result capturing
a fresh, distinct capability, hence the name. Unlike a result `any`, a result `fresh` is isolated:
it cannot be merged with capabilities from the enclosing scope. Note that `fresh` applies only to
function types, not to method return types (which use `any`).

So, when writing `T^` (shorthand for `T^{any}`), `any` is a way of saying "captures something"
without naming what it is precisely, and depending on the context, the capture checker imposes
restrictions on which capabilities are allowed to flow into them by means of subcapturing. Writing
`T^{fresh}` instead says "captures something new and isolated"; see the comparison
[below](#any-vs-fresh-in-function-type-results) for the practical difference.

Another analogy for the different `any`s is that they are some form of implicitly named existential
or abstract self-capture set attached to elements of the program structure, e.g., scopes,
parameters, or return values.

## Local `any`s

Local `any`s form a subcapturing hierarchy based on lexical nesting: a nested scope's local `any`
subsumes its enclosing scope's local `any`. This makes sense because the inner scope can use any
capability available in the outer scope as well as locally defined ones. At the top level, there is
a true universal `any` — the local `any` of the global scope — which all other local `any`s
ultimately subsume:

```scala sc:nocompile
// top level: the global `any`
class Outer: // has local any₁
  val f1: File^ = File("f1") // File^{any₁}
  def method() = // has local any₂
    val f2: File^ = File("f2") // File^{any₂}
    var ref: () => Unit = null // () ->{any₂} Unit, can accept what can flow into any₂
    val closure = () => // has local any₃
      val f3: File^ = File("f3") // File^{any₃}
      val f4: File^ = f2 // ok, because {any₂} <: {any₃}
      val f5: File^ = f1 // ok, because {any₁} <: {any₃}
      ref = () => f3.read() // error, f3 is at the level of any₃ and cannot flow into any₂
      ...
```

Each capability has a _level_ corresponding to the local `any` of its defining scope. The level
determines where a capability can flow: it can flow into `any`s at the same level or more deeply
nested, but not outward to enclosing scopes (which would mean a capability lives longer than its
lexical lifetime). The compiler computes a capability's level by walking up the ownership chain
until reaching a symbol that represents a level boundary. Level boundaries are:
- **Classes**
- **Static objects**
- **Methods**

Local values like `f1`, `f2`, `ref`, etc., don't define their own levels. They inherit the level of
their enclosing method or class. For example, this means:
- `f1` is at `Outer`'s level, i.e., `f1` subcaptures local `any₁`.
- `f2` and `ref` are both at `method`'s level, i.e., both subcapture local `any₂`.
- By lexical nesting, `{any₂} <: {any₃}` holds, but it does **not** hold that `{any₃} <: {any₂}`. Hence,
we cannot assign the closure to `ref`, because `{f3}` is subcapture-bounded by `{any₃}`.

### Charging Captures

When a capability is used, it must be checked for compatibility with the capture-set constraints of
all enclosing scopes. This process is called _charging_ the capability to the environment.

```scala sc:nocompile
def outer(fs: FileSystem^): Unit =
  def inner: () ->{fs} Unit =
    () => fs.read()  // fs is used here
  inner()
```

When the capture checker sees `fs.read()`, it verifies that `fs` can flow into each enclosing scope:
1. The immediately enclosing closure `() => fs.read()` must permit `fs` in its capture set ✓
2. The enclosing method `inner` must account for `fs` (it does, via its capture set) ✓
3. The enclosing method `outer` must account for `fs` (it does, via its parameter) ✓

If any scope refuses to absorb the capability, capture checking fails:

```scala sc:nocompile
def process(fs: FileSystem^): Unit =
  val f: () -> Unit = () => fs.read()  // Error: fs cannot flow into {}
```

The closure is declared pure (`() -> Unit`), meaning its local `any` is the empty set. The
capability `fs` cannot flow into an empty set, so the checker rejects this.

### Visibility and Widening

When capabilities flow outward to enclosing scopes, they must remain visible. A local capability
cannot appear in a type outside its defining scope. In such cases, the capture set is _widened_ to
the smallest visible super capture set:

```scala sc:nocompile
def test(fs: FileSystem^/*{any₁}*/): Logger^{fs} =
  val localLogger = Logger(fs)
  localLogger  // Type widens from Logger^{localLogger} to Logger^{fs}
```

Here, `localLogger` cannot appear in the result type because it's a local variable. The capture set
`{localLogger}` widens to `{fs}` (since `localLogger` captures `fs`), which is visible
outside of `test`.

#### Try-With-Resources, Again

Local `any`s are one of the mechanisms that enable [escape checking](basics.md#escape-checking) for
the try-with-resources pattern. They prevent escaping of scoped capabilities through (direct or
indirect) assignment to mutable variables:
```scala sc:nocompile
def withFile[T](block: File^ => T): T

var esc: File^/*{any₁}*/ = null

withFile: f /* : File^{any₂} */ =>
  esc = f   // error, since any₂ cannot flow into any₁
```

The other mechanism is careful treatment of `any`s in function results
(cf. below) which prevents returning closures holding onto `f`.

Later sections on [capability classifiers](classifiers.md) will add a controlled mechanism that
permits capabilities to escape their level for situations where this would be desirable.

### Local `any`s of Classes

A class receives its own local `any` for the scope of its body. This `any` serves as a template for
a new `any` that will be attached to each instance of the class. Inside the class body, references
to the class's `any` are implicitly prefixed by the path `this`:

```scala sc:nocompile
class Logger(fs: FileSystem^): // local any₁
  // Logger has its own local any₁, accessed as this.any₁
  val file: File^ = fs.open("log.txt")  // File^{this.any₁}
  def log(msg: String): Unit = file.write(msg)
```

When a class inherits from other classes or traits, the `any`s of all supertypes in the `extends`
clause are essentially unified with the `any` of the current class. This unification happens because
all inherited members are accessed through `this`, and hence the local `any`s will conform through
subtyping with each other:

```scala sc:nocompile
trait Super: // local any₁
  val doSomething: () => Unit // () ->{any₁} Unit

class Logger(fs: FileSystem^) extends Super: // local any₂
  val file: File^ = fs.open("log.txt") // File^{any₂}
  def log(msg: String): Unit = file.write(msg)
  val doSomething = () => log("hello") // ok, since {file} <: {this.any₂} =:= {this.any₁}
```

As explained in [Capture Checking of Classes](classes.md), the capture checker infers and verifies
constraints on the contents of a class's `any` through its self-type, reporting any inconsistencies.

When creating an instance, the class's template `any` is substituted with a new `any` specific to
the new object:

```scala sc:nocompile
def test(fs: FileSystem^) = /* local any₁ */
  val logger1 = Logger(fs)  // New logger1.any for this instance, capturing fs
  val logger2 = Logger(fs)  // New logger2.any, distinct from logger1.any
```
Note that the `any`s attached to `logger1` and `logger2` subsume the local `any` of method `test` in accordance
to the rules outlined earlier.

Conceptually, a class' local `any` behaves like an implicit [capture-set member](polymorphism.md#capability-members)
present in the class and all its supertypes:

```scala sc:nocompile
class Logger(fs: FileSystem^) extends Super:
  type Cap^
  val file: File^{Cap} = ...
  // ...
```

## Parameter and Result `any`s in Function Types

So far we've discussed local `any`s that follow the lexical nesting hierarchy. But `any` can also appear in function parameter and result types, where special binding rules apply.

### Existential Binding

Consider this method:

```scala sc:nocompile
def makeLogger(fs: FileSystem^): Logger^ = new Logger(fs)
```

This creates a `Logger` that captures `fs`. We could have been more specific in specifying
`Logger^{fs}` as the return type, but the current definition is also valid, and might be preferable
if we want to hide details of what the returned logger captures. If we write it as above then
certainly the implied `any` in the return type should be able to absorb the capability `fs`. This
means that this `any` has to be defined in a scope in which `fs` is visible.

In logic, the usual way to achieve this scoping is with an existential binder. We can express the
type of `makeLogger` like this:
```scala sc:nocompile
makeLogger: (fs: ∃any₁.FileSystem^{any₁}): ∃fresh. Logger^{fresh}
```
In words: `makeLogger` takes a parameter `fs` of type `Filesystem` capturing _some_ universal
capability `any₁` and returns a `Logger` capturing some (possibly different) `fresh` capability.

We can also turn the existential in the function parameter to a universal "forall" in the function
itself. In that alternative notation, the type of `makeLogger` would read like this:
```scala sc:nocompile
makeLogger: ∀any₁.(fs: FileSystem^{any₁}): ∃fresh. Logger^{fresh}
```
There's a connection with [capture polymorphism](polymorphism.md) here. `any`s in function
parameters behave like additional capture parameters that can be instantiated at the call site to
arbitrary capabilities.

### Expansion Rules for Function Types

The conventions for method types carry over to function types. A function type with `fresh` in the
result, such as
```scala sc:nocompile
(x: T) -> U^{fresh}
```
is interpreted as having an existentially bound `fresh`:
```scala sc:nocompile
(x: T) -> ∃fresh.U^{fresh}
```
The same rules hold for all kinds of function arrows: `->`, `=>`, `?->`, and `?=>`. So `fresh` can in
this case absorb the function parameter `x` since `x` is locally bound in the function result.

Only `fresh` expands to existentially bound capabilities, and it does so regardless of how the
function is written. For instance, both of these types have existentially bound result capabilities:
```scala sc:nocompile
A => B^{fresh}                         // ∃fresh. A ->{any} B^{fresh}
(x: A) -> B -> C^{fresh}               // (x: A) -> ∃fresh. B -> C^{fresh}
```

In effect, we have some form of "second-class" existential capture types which are carefully
restricted and implied by the type structure and `fresh` occurrences. Unlike first-class existential
types, this approach does not require programmers to explicitly pack and unpack: the system
determines the binding structure automatically from where `fresh` appears in the type.

#### Summary

  - Occurrences of `fresh` in a function result type are replaced by a new existential variable
    bound by a quantifier scoping over the result type.
  - If a function parameter type contains covariant occurrences of `any`, we replace these
    occurrences with a new existential variable scoping over the parameter type.
  - Occurrences of `any` in function result types are not translated to existential variables.
    They refer to the local `any` of the enclosing scope. This includes bare `T^` in function type
    results, which is shorthand for `T^{any}`.

#### `any` vs `fresh` in Function Type Results

The rules above establish a key practical distinction when writing function types. Consider:

```scala sc:nocompile
import caps.fresh
class A
class B

def test(): Unit =
  val f: (x: A^) -> B^{fresh} = ???   // B^{fresh}: existentially bound
  val g: A^ -> B^             = ???   // B^{any}: enclosing scope's local any

  val _: A^ -> B^        = f   // error: fresh is not in {any}
  val _: A^ -> B^{fresh} = f   // ok
  val _: (x: A^) -> B^{fresh} = g   // error: g* is not in {fresh}
  val _: A^ -> B^        = g   // ok
```

With `B^{any}` in the result, the returned value's captures are tied to the enclosing scope's local
`any`. Two calls to `g` produce values with the same capture-set bound. The checker treats them
identically. With `B^{fresh}`, each call to `f` yields a value with a distinct existential
capability, so the checker can prove that results from different calls do not alias.

The two types are not interchangeable: a result `fresh` cannot flow into a local `any` (since the
existential is not visible from the enclosing scope), and a local `any` cannot flow into a result
`fresh` (since `fresh` can only subsume other existentials or
[shared capabilities](classifiers.md)).

#### Outer-Bound `fresh`

By default, `fresh` in a function result type is bound by the immediately enclosing function. But
sometimes we want the `fresh` to be bound by an _outer_ function instead. This can be achieved by
using type aliases or capture-set parameters to "tunnel" the `fresh` through an inner function type.

Consider these type definitions:
```scala sc:nocompile
class A
type F[X] = (t: String) -> X
type G[C^] = (t: String) -> A^{C}
```

With these aliases, we can write:
```scala sc:nocompile
val x: (s: String) -> F[A^{fresh}] = ???
val y: (s: String) -> G[{fresh}] = ???
```

In both cases, the `fresh` is bound by the outer function `(s: String) -> ...`, not by the inner
function `(t: String) -> ...`. This works because `fresh` appears outside the inner function type
definition—it is passed as a type argument or capture-set argument to the alias. The expanded types
are:
```scala sc:nocompile
x: ∃fresh. (s: String) -> (t: String) -> A^{fresh}
y: ∃fresh. (s: String) -> (t: String) -> A^{fresh}
```

This technique is useful when a capability needs to span nested function calls while remaining
existentially bound at an outer scope.

### Parameter `any`s and Local `any`s

Inside the function body, parameter `any`s are at the **same level** as the function's local `any`.
This means the function's local `any` can subsume capabilities from parameters:

```scala sc:nocompile
def process(x: File^/* parameter {any₁} */): Unit = /* local any₂ */
  val y: File^/*{any₂}*/ = x  // OK: x's any is at process's level, same as process's local any
  val f: () =>/*{any₂}*/ Unit = () => x.read()  // OK: closure's local any subsumes x
```

The parameter `x` has a capability at `process`'s level. The local `any` of `process` (and any
nested closures) can subsume it because they're at the same level or more deeply nested.

### Result `fresh`s are Isolated

Result `fresh`s (i.e., those we assign an existential capture set in function-result types) cannot
absorb capabilities that would allow scoped resources to escape. Consider trying to leak a file by
directly returning a closure that captures it:

```scala sc:nocompile
withFile[() => File^]("test.txt"): f =>
//       ^^^^^^^^^^^ T = () => File^, i.e., () ->{any} File^{any} for some outer any
  () => f  // We want to return this as () => File^
```

The lambda `(f: File^) => () => f` has inferred type:
```scala sc:nocompile
(f: File^) -> () ->{f} File^{f}
```
The inner closure explicitly captures `f`. To fit the expected type `File^ => () => File^`, we'd
need to widen through these steps:

```scala sc:nocompile
(f: File^) -> () ->{f} File^{f}         // inferred: captures f explicitly
(f: File^) -> () ->{any} File^{any}     // widen to any
(f: File^) -> ∃fresh. () ->{fresh} File^{fresh} // apply existential rule for result `fresh`
```

The final step produces a result `fresh` that is existentially bound to the outer function type. But
the expected type `() => File^` has an `any` bound to the enclosing scope of `withFile` outside the
function entirely. The capture checker prevents the existentially bound `fresh` from flowing
into this outer `any`, so the assignment fails.

Otherwise, allowing widening `∃fresh. () ->{fresh} File^{fresh}` to `() => File^` would let the scoped file escape:

```scala sc:nocompile
val escaped: () => File^ = withFile[() => File^]("test.txt")(f => () => f)
//           ^^^^^^^^^^^ any here is in the outer scope
escaped().read()  // Use-after-close!
```

By keeping result `fresh`s isolated, the capture checker ensures that an existential cannot hide `f` and
smuggle it out of its scope.

Beyond preventing escaping capabilities, the principle of isolating result `fresh`s is important for
[tracking mutation and allocation effects](mutability.md) and
[separation checking](separation-checking.md), e.g., for a function returning a new mutable
reference cell on each call
```scala sc:nocompile
def freshCell(init: Int): Cell^ = new Cell(s)
val c1 = freshCell(0).set(42) // Cell^{fresh₁}
val c2 = freshCell(11)        // Cell^{fresh₂}, fresh₁ and fresh₂ are incomparable
```
a natural type would be `(init: Int) -> Cell^`, i.e., `(init: Int) -> ∃fresh.Cell[Int]^{fresh}`
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