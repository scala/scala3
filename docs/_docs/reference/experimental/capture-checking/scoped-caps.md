---
layout: doc-page
title: "Scoped Caps"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/scoped-caps.html
---

## Scoped Universal Capabilities

When discussing escape checking, we referred to a scoping discipline. That is, capture sets can contain only capabilities that are visible at the point where the set is defined. But that raises the question: where is a universal capability `cap` defined? In fact, what is written as the top type `cap` can mean different capabilities, depending on scope. Usually a `cap` refers to a universal capability defined in the scope where the `cap` appears.

A useful mental model is to think of `cap` as a "container" that can _absorb_ concrete capabilities. When you write `T^` (shorthand for `T^{cap}`), you're saying "this value may capture some capabilities that will flow into this `cap`." Different `cap` instances in different scopes are different containers: a capability that flows into one doesn't automatically flow into another. We will further expand on this idea later when discussing [separation checking](separation-checking.md).

### Existential Binding

Special rules apply to `cap`s in method and function parameters and results. For example, take this method:

```scala
def makeLogger(fs: FileSystem^): Logger^ = new Logger(fs)
```

This creates a `Logger` that captures `fs`. We could have been more specific in specifying `Logger^{fs}` as the return type, but the current definition is also valid, and might be preferable if we want to hide details of what the returned logger captures. If we write it as above then certainly the implied `cap` in the return type should be able to absorb the capability `fs`. This means that this `cap` has to be defined in a scope in which `fs` is visible.

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

The conventions for method types carry over to function types. A dependent function type
```scala
(x: T) -> U^
```
is interpreted as having an existentially bound `cap` in the result, like this:
```scala
(x: T) -> ∃cap.U^{cap}
```
The same rules hold for the other kinds of function arrows, `=>`, `?->`, and `?=>`. So `cap` can in this case absorb the function parameter `x` since `x` is locally bound in the function result.

However, the expansion of `cap` into an existentially bound variable only applies to functions that use the dependent function style syntax, with explicitly named parameters. Parametric functions such as `A => B^` or `(A₁, ..., Aₖ) -> B^` don't bind the `cap` in their return types in an existential quantifier. For instance, the function
```scala
(x: A) -> B -> C^
```
is interpreted as
```scala
(x: A) -> ∃cap.B -> C^{cap}
```
In other words, existential quantifiers are only inserted in results of function arrows that follow an explicitly named parameter list.

**Examples:**

 - `A => B` is an alias type that expands to `A ->{cap} B`.
 -  Therefore
   `(x: T) -> A => B` expands to `(x: T) -> ∃c.(A ->{c} B)`.

 - `(x: T) -> Iterator[A => B]` expands to `(x: T) -> ∃c.Iterator[A ->{c} B]`

To summarize:

  - If a function result type follows a named parameter list and contains covariant occurrences of `cap`,
    we replace these occurrences with a fresh existential variable which
    is bound by a quantifier scoping over the result type.
  - If a function parameter type contains covariant occurrences of `cap`, we replace these occurrences with
    a fresh existential variable scoping over the parameter type.
  - Occurrences of `cap` elsewhere are not translated. They can be seen as representing an existential in the
    scope of the definition in which they appear.

## Levels and Escape Prevention

Each capability has a _level_ corresponding to where it was defined. The level determines where a capability can flow: it can flow into `cap`s at the same level or more deeply nested, but not outward to enclosing scopes (which would mean a capability lives longer than its lexical lifetime). Later sections on [capability classifiers](classifiers.md) will add a controlled mechanism that permits escaping/flowing outward for situations
where this would be desirable.

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
    ref.set(f)                           // Error: c2 would escape its level

    class Local:                       // level: Local
      def method(c3: Cap^) =             // level: method
        val z = c3                         // level: method
```

Local values like `x`, `y`, and `z` don't define their own levels. They inherit the level of their enclosing method or class. This means:
- `c1` and `ref` are both at `outer`'s level
- `c2` and `f` are both at `inner`'s level
- `c3` and `z` are both at `method`'s level

### The Level Check

A capability can flow into a `cap` only if that `cap`'s scope is _contained in_ the capability's level owner. In the example above, `ref.set(f)` fails because:
- `ref`'s type parameter has a `cap` that was instantiated at `outer`'s level
- `f` captures `c2`, which is at `inner`'s level
- `outer` is not contained in `inner`, so `c2` cannot flow into `ref`'s `cap`

This ensures capabilities flow "inward" to more nested scopes, never "outward" to enclosing ones.

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
// Scala CC: rejected because c escapes inner's level to outer's level
def outer() =
  var escape: () => Unit = () => ()
  def inner(c: Cap^) =
    escape = () => c.use()  // Error: c at inner's level cannot escape to outer
  inner(Cap())
  escape
```

The key analogies are:
- **Levels ≈ Lifetimes**: Both represent "how long something is valid"
- **Containment ≈ Outlives**: Rust's `'a: 'b` (a outlives b) corresponds to Scala's level containment check (but inverted: inner scopes are contained in outer ones)
- **Escape prevention**: Both reject code where a reference/capability would outlive its scope

The key differences are:
- **What's tracked**: Rust tracks memory validity (preventing dangling pointers). Scala CC tracks capability usage (preventing unauthorized effects).
- **Explicit vs. implicit**: Rust lifetimes are often written explicitly (`&'a T`). Scala capture checking levels are computed automatically from the program structure.

## Charging Captures to Enclosing Scopes

When a capability is used, it must flow into the `cap`s of all enclosing scopes. This process is
called _charging_ the capability to the environment.

```scala
def outer(fs: FileSystem^): Unit =
  def inner(): () ->{fs} Unit =
    () => fs.read()  // fs is used here
  inner()
```

When the capture checker sees `fs.read()`, it verifies that `fs` can flow into each enclosing scope:
1. The immediately enclosing closure `() => fs.read()` must have `fs` in its capture set ✓
2. The enclosing method `inner` must account for `fs` (it does, via its result type) ✓
3. The enclosing method `outer` must account for `fs` (it does, via its parameter) ✓

If any scope refuses to absorb the capability, capture checking fails:

```scala
def process(fs: FileSystem^): Unit =
  val f: () -> Unit = () => fs.read()  // Error: fs cannot flow into {}
```

The closure is declared pure (`() -> Unit`), meaning its `cap` is the empty set. The capability `fs` cannot flow into an empty set, so this is rejected.

## Visibility and Widening

When capabilities flow outward to enclosing scopes, they must remain visible. A local capability cannot appear in a type outside its defining scope. In such cases, the capture set is _widened_ to the smallest visible super capture set:

```scala
def test(fs: FileSystem^): Logger^{fs} =
  val localLogger = Logger(fs)
  localLogger  // Type widens from Logger^{localLogger} to Logger^{fs}
```

Here, `localLogger` cannot appear in the result type because it's a local variable. The capture set `{localLogger}` widens to `{fs}`, which covers it (since `localLogger` captures `fs`) and is visible outside `test`. In effect, `fs` flows into the result's `cap` instead of `localLogger`.

However, widening cannot always find a valid target:

```scala
def test(fs: FileSystem^): () -> Unit =
  val localLogger = Logger(fs)
  () => localLogger.log("hello")  // Error: cannot widen to empty set
```

The closure captures `localLogger`, but the return type `() -> Unit` has an empty capture set. There's no visible capability that covers `localLogger` and can flow into an empty set, so the checker rejects this code.
