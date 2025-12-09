---
layout: doc-page
title: "Scoped Caps"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/scoped-caps.html
---

## Scoped Universal Capabilities

When discussing escape checking, we referred to a scoping discipline. That is, capture sets can contain only capabilities that are visible at the point where the set is defined. But that raises the question: where is a universal capability `cap` defined? In fact, what is written as the top type `cap` can mean different capabilities, depending on scope. Usually a `cap` refers to a universal capability defined in the scope where the `cap` appears.

### Existential Binding

Special rules apply to `cap`s in method and function parameters and results. For example, take this method:

```scala
def makeLogger(fs: FileSystem^): Logger^ = new Logger(fs)
```

This creates a `Logger` that captures `fs`. We could have been more specific in specifying `Logger^{fs}` as the return type, but the current definition is also valid, and might be preferable if we want to hide details of what the returned logger captures. If we write it as above then certainly the implied `cap` in the return type should be able to subsume the capability `fs`. This means that this `cap` has to be defined in a scope in which `fs` is visible.

In logic, the usual way to achieve this scoping is with an existential binder. We can express the type of `makeLogger` like this:
```scala
makeLogger: (fs: ∃cap₁.FileSystem^{cap₁}): ∃cap₂. Logger^{cap₂}
```
In words: `makeLogger` takes a parameter `fs` of type `Filesystem` capturing _some_ universal capability `cap₁` and returns a `Logger` capturing some other (possibly different) universal `cap₂`.

We can also turn the existential in the function parameter to a universal "forall" in the function itself. In that alternative notation, the type of makeLogger would read like this:
```scala
makeLogger: ∀cap₁.(fs: FileSystem^{cap₁}): ∃cap₂. Logger^{cap₂}
```
There's a connection with [capture polymorphism](polymorphism.md) here. `cap`s in function parameters behave like additional capture parameters that can be instantiated at the call site to arbitrary capabilities.

### Function Types

The conventions for method types carry over to function types. A function type
```scala
(x: T) -> U^
```
is interpreted as having an existentially bound `cap` in the result, like this:
```scala
(x: T) -> ∃cap.U^{cap}
```
The same rules hold for the other kinds of function arrows, `=>`, `?->`, and `?=>`. So `cap` can in this case subsume the function parameter `x` since it is locally bound in the function result.

However, the expansion of `cap` into an existentially bound variable only applies to functions that use the dependent function style syntax, with explicitly named parameters. Parametric functions such as `A => B^` or `(A₁, ..., Aₖ) -> B^` don't bind their result cap in an existential quantifier. For instance, the function
```scala
(x: A) -> B -> C^
```
is interpreted as
```scala
(x: A) -> ∃cap.B -> C^{cap}
```
In other words, existential quantifiers are only inserted in results of function arrows that follow an explicitly named parameter list.

**Examples:**

 - `A => B` is an alias type that expands to `A ->{cap} B`, therefore
   `(x: T) -> A => B` expands to `(x: T) -> ∃cap.(A ->{cap} B)`.

 - `(x: T) -> Iterator[A => B]` expands to `(x: T) -> ∃cap.Iterator[A ->{cap} B]`

To summarize:

  - If a function result type follows a named parameter list and contains covariant occurrences of `cap`,
    we replace these occurrences with a fresh existential variable which
    is bound by a quantifier scoping over the result type.
  - If a function parameter type contains covariant occurrences of `cap`, we replace these occurrences with
    a fresh existential variable scoping over the parameter type.
  - Occurrences of `cap` elsewhere are not translated. They can be seen as representing an existential in the
    scope of the definition in which they appear.

### Fresh Capabilities vs Result Capabilities

Internally, the compiler represents scoped `cap` instances using two different mechanisms:

- **Fresh capabilities** are used for most `cap` instances. They track a _hidden set_ of concrete capabilities they subsume. When you pass a `FileSystem^` to a function expecting `T^`, the fresh capability in the parameter learns that it subsumes your specific `FileSystem`. Fresh capabilities participate in subcapturing: if `{fs} <: {cap}`, the fresh capability records `fs` in its hidden set.

- **Result capabilities** are used for `cap` in dependent function results. They are _rigid_—they don't accumulate a hidden set. Instead, two result capabilities can only be related through _unification_, which makes them equivalent. This prevents the result's capture set from being "polluted" by unrelated capabilities.

The distinction matters when checking function subtyping:

```scala
val f: (x: FileSystem^) -> Logger^ = ???
val g: (x: FileSystem^) -> Logger^{x} = f  // OK
```

Here, the result `cap` in `f`'s type is a result capability. When checking if `f` can be assigned to `g`, the checker unifies `f`'s result capability with `{x}`. This works because unification is symmetric—we're just saying "these represent the same capability."

In contrast:

```scala
val leaky: Logger^ = ???
val f: (x: FileSystem^) -> Logger^ = x => leaky  // Error
```

This fails because `leaky`'s capture set cannot flow into the result capability—result capabilities don't accept arbitrary capabilities through subsumption, only through unification with other result capabilities tied to the same function.

## Levels and Escape Prevention

Each capability has a _level_ corresponding to where it was defined. A capability can only be captured by scopes at the same level or nested more deeply.

### How Levels Are Computed

A capability's level is determined by its _level owner_, which the compiler computes by walking up the ownership chain until reaching a symbol that represents a level boundary. Level boundaries are:
- **Classes** (but not inner non-static module classes)
- **Methods** (but not accessors or constructors)

Consider this example:

```scala
def outer(c1: Cap^) =                // level: outer
  val x = 1                            // level: outer (vals don't create levels)
  val ref = Ref[() => Unit](() => ())

  def inner(c2: Cap^) =                // level: inner
    val y = 2                            // level: inner
    val f = () => c2.use()
    ref.set(f)                           // Error: cap2 would escape its level

    class Local:                       // level: Local
      def method(c3: Cap^) =             // level: method
        val z = c3                         // level: method
```

Local values like `x`, `y`, and `z` don't define their own levels. They inherit the level of their enclosing method or class. This means:
- `c1` and `ref` are both at `outer`'s level
- `c2` and `f` are both at `inner`'s level
- `c3` and `z` are both at `method`'s level

### The Level Check

A capability can flow into a capture set only if the capture set's scope is _contained in_ the capability's level owner. In the example above, `ref.set(f)` fails because:
- `ref`'s type parameter was instantiated at `outer`'s level
- `f` captures `cap2`, which is at `inner`'s level
- `outer` is not contained in `inner`, so `cap2` cannot flow into `ref`

This ensures capabilities can only flow "inward" to more nested scopes, never "outward" to enclosing ones.

### Comparison with Rust Lifetimes

Readers familiar with Rust may notice similarities to lifetime checking. Both systems prevent references from escaping their valid scope:

```rust
// Rust: rejected because x doesn't live long enough
fn bad<'a>() -> &'a i32 {
    let x = 42;
    &x
}
```

```scala
// Scala CC: rejected because cap would escape its level
def bad(): () -> Unit =
  val cap = CC()
  () => cap.use()
```

The key analogies are:
- **Levels ≈ Lifetimes**: Both represent "how long something is valid"
- **Containment ≈ Outlives**: Rust's `'a: 'b` (a outlives b) corresponds to Scala's level containment check (but inverted: inner scopes are contained in outer ones)
- **Escape prevention**: Both reject code where a reference/capability would outlive its scope

The key differences are:
- **What's tracked**: Rust tracks memory validity (preventing dangling pointers). Scala CC tracks capability usage (preventing unauthorized effects).
- **Explicit vs. implicit**: Rust lifetimes are often written explicitly (`&'a T`). Scala CC levels are computed automatically from the program structure.
- **Granularity**: Rust lifetimes can distinguish different fields of a struct. Scala CC levels are coarser, tied to method and class boundaries.

## Charging Captures to Enclosing Scopes

When a capability is used, the capture checker must verify that all enclosing scopes properly account for it. This process is called _charging_ the capability to the environment.

```scala
def outer(cap1: FileSystem^): Unit =
  def inner(): () ->{cap1} Unit =
    () => cap1.read()  // cap1 is used here
  inner()
```

When the capture checker sees `cap1.read()`, it verifies that:
1. The immediately enclosing closure `() => cap1.read()` declares `cap1` in its capture set
2. The enclosing method `inner` accounts for `cap1` (it does, via its result type)
3. The enclosing method `outer` accounts for `cap1` (it does, via its parameter)

At each level, the checker verifies that the used capabilities are a _subcapture_ of what the scope declares:

```scala
def process(fs: FileSystem^): Unit =
  val f: () -> Unit = () => fs.read()  // Error: {fs} is not a subset of {}
```

The closure is declared pure (`() -> Unit`), but uses `fs`. Since `{fs}` is not a subset of the empty capture set, capture checking fails.

## Visibility and Widening

As capabilities are charged to outer scopes, they are _filtered_ to include only those visible at each level. When a local capability cannot appear in a type (because it's not visible), the capture set is _widened_ to the smallest visible superset:

```scala
def test(fs: FileSystem^): Logger^ =
  val localLogger = Logger(fs)
  localLogger  // Type is widened from Logger^{localLogger} to Logger^{fs}
```

Here, `localLogger` cannot appear in the result type because it's a local variable. The capture set `{localLogger}` is widened to `{fs}`, which covers it (since `localLogger` captures `fs`) and is visible outside `test`.

However, widening cannot always succeed:

```scala
def test(fs: FileSystem^): () -> Unit =
  val localLogger = Logger(fs)
  () => localLogger.log("hello")  // Error: cannot widen to empty set
```

The closure's capture set `{localLogger}` would need to be widened to fit the return type `() -> Unit`, but there's no visible capability that covers `localLogger` and fits in the empty set. This is an error.
