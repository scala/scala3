# Exclusive Capabilities

Language design draft


## Capability Kinds

A capability is called
  - _exclusive_ if it is `cap` or it has an exclusive capability in its capture set.
  - _shared_ otherwise.

There is a new top capability `shared` which can be used as a capability for deriving shared capture sets. Other shared capabilities are created as read-only versions of exclusive capabilities.

## Update Methods

We introduce a new trait
```scala
trait Mutable
```
It is used as a base trait for types that define _update methods_ using
a new modifier `mut`.

`mut` can only be used in classes or objects extending `Mutable`. An update method is allowed to access exclusive capabilities in the method's environment. By contrast, a normal method in a type extending `Mutable` may access exclusive capabilities only if they are  defined locally or passed to it in parameters.

**Example:**
```scala
class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  mut def put(x: Int): Unit = current = x
```
Here, `put` needs to be declared as an update method since it accesses the exclusive write capability of the variable `current` in its environment.
`mut` can also be used on an inner class of a class or object extending `Mutable`. It gives all code in the class the right
to  access exclusive capabilities in the class environment. Normal classes
can only access exclusive capabilities defined in the class or passed to it in parameters.

```scala
object Registry extends Mutable:
  var count = 0
  mut class Counter:
    mut def next: Int =
      count += 1
      count
```
Normal method members of `Mutable` classes cannot call update methods. This is indicated since accesses in the callee are recorded in the caller. So if the callee captures exclusive capabilities so does the caller.

An update method cannot implement or override a normal method, whereas normal methods may implement or override update methods. Since methods such as `toString` or `==` inherited from Object are normal methods, it follows that none of these methods may be implemented as an update method.

The `apply` method of a function type is also a normal method, hence `Mutable` classes may not implement a function type with an update method as the `apply` method.

## Mutable Types

A type is called a _mutable_ if it extends `Mutable` and it has an update method or an update class as non-private member or constructor.

When we create an instance of a mutable type we always add `cap` to its capture set. For instance, if class `Ref` is declared as shown previously then `new Ref(1)` has type `Ref[Int]^{cap}`.

**Restriction:** A non-mutable type cannot be downcast by a pattern match to a mutable type.

**Definition:** A class is _read_only_ if the following conditions are met:

 1. It does not extend any exclusive capabilities from its environment.
 2. It does not take parameters with exclusive capabilities.
 3. It does not contain mutable fields, or fields that take exclusive capabilities.

**Restriction:** If a class or trait extends `Mutable` all its parent classes or traits must either extend `Mutable` or be read-only.

The idea is that when we upcast a reference to a type extending `Mutable` to a type that does not extend `Mutable`, we cannot possibly call a method on this reference that uses an exclusive capability. Indeed, by the previous restriction this class must be a read-only class, which means that none of the code implemented
in the class can access exclusive capabilities on its own. And we
also cannot override any of the methods of this class with a method
accessing exclusive capabilities, since such a method would have
to be an update method and update methods are not allowed to override regular methods.



**Example:**

Consider trait `IterableOnce` from the standard library.

```scala
trait IterableOnce[+T] extends Mutable:
  def iterator: Iterator[T]^{this}
  mut def foreach(op: T => Unit): Unit
  mut def exists(op: T => Boolean): Boolean
  ...
```
The trait is a mutable type with many update methods, among them `foreach` and `exists`. These need to be classified as `mut` because their implementation in the subtrait `Iterator` uses the update method `next`.
```scala
trait Iterator[T] extends IterableOnce[T]:
  def iterator = this
  def hasNext: Boolean
  mut def next(): T
  mut def foreach(op: T => Unit): Unit = ...
  mut def exists(op; T => Boolean): Boolean = ...
  ...
```
But there are other implementations of `IterableOnce` that are not mutable types (even though they do indirectly extend the `Mutable` trait). Notably, collection classes implement `IterableOnce` by creating a fresh
`iterator` each time one is required. The mutation via `next()` is then restricted to the state of that iterator, whereas the underlying collection is unaffected. These implementations would implement each `mut` method in `IterableOnce` by a normal method without the `mut` modifier.

```scala
trait Iterable[T] extends IterableOnce[T]:
  def iterator = new Iterator[T] { ... }
  def foreach(op: T => Unit) = iterator.foreach(op)
  def exists(op: T => Boolean) = iterator.exists(op)
```
Here, `Iterable` is not a mutable type since it has no update method as member.
All inherited update methods are (re-)implemented by normal methods.

**Note:** One might think that we don't need a base trait `Mutable` since in any case
a mutable type is defined by the presence of update methods, not by what it extends. In fact the importance of `Mutable` is that it defines _the other methods_ as read-only methods that _cannot_ access exclusive capabilities. For types not extending `Mutable`, this is not the case. For instance, the `apply` method of a function type is not an update method and the type itself does not extend `Mutable`. But `apply` may well be implemented by
a method that accesses exclusive capabilities.



## Read-only Capabilities

If `x` is an exclusive capability of a type extending `Mutable`, `x.rd` is its associated, shared _read-only_ capability.

`shared` can be understood as the read-only capability corresponding to `cap`.
```scala
  shared = cap.rd
```

A _top capability_ is either `cap` or `shared`.


## Shorthands

**Meaning of `^`:**

The meaning of `^` and `=>` is the same as before:

 - `C^` means `C^{cap}`.
 - `A => B` means `(A -> B)^{cap}`.

**Implicitly added capture sets**

A reference to a type extending any of the traits `Capability` or `Mutable` gets an implicit capture set `{shared}` in case no explicit capture set is given.

For instance, a matrix multiplication method can be expressed as follows:

```scala
class Matrix(nrows: Int, ncols: Int) extends Mutable:
  mut def update(i: Int, j: Int, x: Double): Unit = ...
  def apply(i: Int, j: Int): Double = ...

def mul(a: Matrix, b: Matrix, c: Matrix^): Unit =
  // multiply a and b, storing the result in c
```
Here, `a` and `b` are implicitly read-only, and `c`'s type has capture set `cap`. I.e. with explicit capture sets this would read:
```scala
def mul(a: Matrix^{shared}, b: Matrix^{shared}, c: Matrix^{cap}): Unit
```
Separation checking will then make sure that `a` and `b` must be different from `c`.


## Capture Sets

As the previous example showed, we would like to use a `Mutable` type such as `Array` or `Matrix` in two permission levels: read-only and unrestricted. A standard technique is to invent a type qualifier such as "read-only" or "mutable" to indicate access permissions. What we would like to do instead is to combine the qualifier with the capture set of a type. So we
distinguish two kinds of capture sets: regular and read-only. Read-only sets can contain only shared capabilities.

Internally, in the discussion that follows we use a label after the set to indicate its mode. `{...}_` is regular and `{...}rd` is read-only. We could envisage source language to specify read-only sets, e.g. something like

```scala
{io, async}.rd
```

But in almost all cases we don't need an explicit mode in source code to indicate the kind of capture set, since the contents of the set itself tell us what kind it is. A capture set is assumed to be read-only if it is on a
type extending `Mutable` and it contains only shared capabilities, otherwise it is assumed to be regular.

The read-only function `ro` maps capture sets to read-only capture sets. It is defined pointwise on capabilities as follows:

 - `ro ({ x1, ..., xn } _) = { ro(x1), ..., ro(xn) }`
 - `ro(x)    =  x` if `x` is shared
 - `ro(x)    =  x.rd` if `x` is exclusive



## Subcapturing

Subcapturing has to take the mode of capture sets into account. We let `m` stand for arbitrary modes.

1. Rule (sc-var) comes in two variants. If `x` is defined as `S^C` then

   - `{x, xs} m <: (C u {xs}) m`
   - `{x.rd, xs} m <: (ro(C) u {xs}) m`

3. The subset rule works only between sets of the same kind:

   - `C _ <: C _ u {x}`
   - `C rd <: C rd u {x}` if `x` is a shared capability.

4. We can map regular capture sets to read-only sets:

   - `C _ <: ro(C) rd`

5. Read-only capabilities in regular capture sets can be widened to exclusive capabilities:

   - `{x.rd, xs} _ <: {x, xs} _`

One case where an explicit capture set mode would be useful concerns
refinements of type variable bounds, as in the following example.
```scala
class A:
  type T <: Object^{x.rd, y}
class B extends A:
  type T <: Object^{x.rd}
class C extends B:
  type T = Matrix^{x.rd}
```
We assume that `x` and `y` are exclusive capabilities.
The capture set of type `T` in class `C` is a read-only set since `Matrix` extends `Mutable`. But the capture sets of the occurrences of
`T` in `A` and `B` are regular. This leads to an error in bounds checking
the definition of `T` in `C` against the one in `B`
since read-only sets do not subcapture regular sets. We can fix the
problem by declaring the capture set in class `B` as read-only:
```scala
class B extends A:
  type T <: Object^{x.rd}.rd
```
But now a different problem arises since the capture set of `T` in `B` is
read-only but the capture set of `T` and `A` is regular. The capture set of
`T` in `A` cannot be made read-only since it contains an exclusive capability `y`. So we'd have to drop `y` and declare class `A` like this:
```scala
class A:
  type T <: Object^{x.rd}.rd
```

<!--
**Examples:**

Say we have

```
type S <: SharedCapability
type T
type M <: Mutable
a: S^{shared}
b: T^{shared}
c: T2^{cap}
d: T3^{b, c}
```
Then

 - `{a} <: {shared}`  by (1)
 - `{d} <: {b, c}` by (1)
 - `{c.rd} <: {shared}` by (2)
 - `{d.rd} <: {b.rd, c.rd}` by (2)
 - `{b.rd} <: {b.rd, c.rd}` by (3)
 - `{b} <: {b, c.rd}` by (3)
 - `{b} <: {b, c}` by (3)
 - `{a} <: {a, b}>` by (3)
 - We do _not_ have: `T1^{b.rd} <: T1^{b.rd, c}` since that would change the capture set from read-only to exclusive.
 - `T2^{cap} <: T2^{shared}`  by (4)
 - `T3^{b, c.rd} <: T3^{b.rd, c.rd}` by (4)
 - `T3^{b.rd, c} <: T3^{b, c}` by (5)
 - S
 - We do _not_ have: `{b.rd} <: {b}` nor do we have `{b.rd, a} <: {b, a}` since that would in each case change the capture set from shared to exclusive.

-->

## Accesses to Mutable Types

A _read-only access_ is a reference `x` to a type extending `Mutable` with a regular capture set if the expected type is one of the following:

 - a value type that is not a mutable type, or
 - a select prototype with a member that is a normal method or class (not an update method or class).

A read-only access contributes the read-only capability `x.rd` to its environment (as formalized by _cv_). Other accesses contribute the full capability `x`.

A reference `p.m` to an update method or class `m` of a mutable type is allowed only if `p`'s capture set is regular.

If `e` is an expression of a type `T^cs` extending `Mutable` and the expected type is a value type that is not a mutable type, then the type of `e` is mapped to `T^ro(cs)`.


## Expression Typing

An expression's type should never contain a top capability in its deep capture set. This is achieved by the following rules:

 - On var access `x`:

    - replace all direct capture sets with `x`
    - replace all boxed caps with `x*`

   _Variant_: If the type of the typevar corresponding to a boxed cap can be uniquely reached by a path `this.p`, replace the `cap` with `x.p*`.

 - On select `t.foo` where `C` is the capture set of `t`: apply the SELECT rule, which amounts to:

   - replace all direct caps with `C`
   - replace all boxed caps with `C*`

 - On applications: `t(args)`, `new C(args)` if the result type `T` contains `cap` (deeply):

   - create a fresh skolem `val sk: T`
   - set result type to `sk.type`

   Skolem symbols are eliminated before they reach the type of the enclosing val or def.

 - When avoiding a variable in a local block, as in:
   ```scala
      { val x: T^ = ...; ... r: List[T^{x}] }
   ```
   where the capture set of `x` contains a top capability,
   replace `x` by a fresh skolem `val sk: T`. Alternatively: keep it as is, but don't widen it.


## Post Processing Right Hand Sides

The type of the right hand sides of `val`s or `def`s is post-processed before it becomes the inferred type or is compared with the declared type. Post processing
means that all local skolems in the type are avoided, which might mean `cap` can now occur in the the type.

However, if a local skolem `sk` has `cap` as underlying type, but is only used
in its read-only form `sk.rd` in the result type, we can drop the skolem instead of widening to `shared`.

**Example:**

```scala
  def f(x: Int): Double = ...

  def precomputed(n: Int)(f: Int -> Double): Int -> Double =
    val a: Array[Double]^ = Array.tabulate(n)(f)
    a(_)
```
Here, `Array.tabulate(n)(f)` returns a value of type `Array[Double]^{cap}`.
The last expression `a(_)` expands to the closure `idx => a(idx)`, which
has type `Int ->{a.rd} Double`, since `a` appears only in the context of a
selection with the `apply` method of `Array`, which is not an update method. The type of the enclosing block then has type `Int ->{sk.rd} Double` for a fresh skolem `sk`,
since `a` is no longer visible. After post processing, this type becomes
`Int -> Double`.

This pattern allows to use mutation in the construction of a local data structure, returning a pure result when the construction is done. Such
data structures are said to have _transient mutability_.

## Separation checking

Separation checking checks that we don't have hidden aliases. A hidden alias arises when we have two definitions `x` and `y` with overlapping transitive capture sets that are not manifest in the types of `x` and `y` because one of these types has widened the alias to a top capability.

Since expression types can't mention cap, widening happens only
 - when passing an argument to a parameter
 - when widening to a declared (result) type of a val or def

**Definitions:**

 - The _transitive capture set_ `tcs(c)` of a capability `c` with underlying capture set `C` is `c` itself, plus the transitive capture set of `C`, but excluding `cap` or `shared`.

 - The _transitive capture set_ `tcs(C)` of a capture set C is the union
   of `tcs(c)` for all elements `c` of `C`.

 - Two capture sets _interfere_ if one contains an exclusive capability `x` and the other
   also contains `x` or contains the read-only capability `x.rd`.

 - If `C1 <: C2` and `C2` contains a top capability, then let `C2a` be `C2` without top capabilities. The hidden set `hidden(C1, C2)` of `C1` relative to `C2` is the smallest subset `C1h` of `C1` such that `C1 \ C1h <: C2a`.

 - If `T1 <: T2` then let the hidden set `hidden(T1, T2)` of `T1` relative to `T2` be the
   union of all hidden sets of corresponding capture sets in `T1` and `T2`.


**Algorithm outline:**

 - Associate _shadowed sets_ with blocks, template statement sequences, applications, and val symbols. The idea is that a shadowed set gets populated when a capture reference is widened to cap. In that case the original references that were widened get added to the set.

 - After processing a `val x: T2 = t` with `t: T1` after post-processing:

   - If `T2` is declared, add `tcs(hidden(T1, T2))` to the shadowed set
   of the enclosing statement sequence and remember it as `shadowed(x)`.
   - If`T2` is inferred, add `tcs(T1)` to the shadowed set
   of the enclosing statement sequence and remember it as `shadowed(x)`.

 - When processing the right hand side of a `def f(params): T2 = t` with `t: T1` after post-processing

   - If `T2` is declared, check that `shadowed*(hidden(T1, T2))` contains only local values (including skolems).
   - If `T2` is inferred, check that `shadowed*(tcs(T1))` contains only local values (including skolems).

   Here, `shadowed*` is the transitive closure of `shadowed`.

 - When processing an application `p.f(arg1, ..., arg_n)`, after processing `p`, add its transitive capture set to the shadowed set of the call. Then, in sequence, process each argument by adding `tcs(hidden(T1, T2))` to the shadowed set of the call, where `T1` is the argument type and `T2` is the type of the formal parameter.

 - When adding a reference `r` or capture set `C` in `markFree` to enclosing environments, check that `tcs(r)` (respectively, `tcs(C)`) does not interfere with an enclosing shadowed set.


This requires, first, a linear processing of the program in evaluation order, and, second, that all capture sets are known. Normal rechecking violates both of these requirements. First, definitions
without declared result types are lazily rechecked using completers. Second, capture sets are constructed
incrementally. So we probably need a second scan after rechecking proper. In order not to duplicate work, we need to record during rechecking all additions to environments via `markFree`.

**Notes:**

 - Mutable variables are not allowed to have top capabilities in their deep capture sets, so separation checking is not needed for checking var definitions or assignments.

 - A lazy val can be thought of conceptually as a value with possibly a capturing type and as a method computing that value. A reference to a lazy val is interpreted as a call to that method. It's use set is the reference to the lazy val itself as well as the use set of the called method.

 -

## Escape Checking

The rules for separation checking also check that capabilities do not escape. Separate
rules for explicitly preventing cap to be boxed or unboxed are not needed anymore. Consider the canonical `withFile` example:
```scala
def withFile[T](body: File^ => T): T =
  ...

withFile: f =>
  () => f.write("too late")
```
Here, the argument to `withFile` has the dependent function type
```scala
(f: File^) -> () ->{f} Unit
```
A non-dependent type is required so the expected result type of the closure is
```
() ->{cap} Unit
```
When typing a closure, we type an anonymous function. The result type of that function is determined by type inference. That means the generated closure looks like this
```scala
{ def $anon(f: File^): () ->{cap} Unit =
    () => f.write("too late")
  $anon
}
```
By the rules of separation checking the hidden set of the body of $anon is `f`, which refers
to a value outside the rhs of `$anon`. This is illegal according to separation checking.

In the last example, `f: File^` was an exclusive capability. But it could equally have been a shared capability, i.e. `withFile` could be formulated as follows:
```scala
def withFile[T](body: File^{shared} => T): T =
```
The same reasoning as before would enforce that there are no leaks.


## Mutable Variables

Local mutable variables are tracked by default. It is essentially as if a mutable variable `x` was decomposed into a new private field of class `Ref` together with a getter and setter. I.e. instead of
```scala
var x: T = init
```
we'd deal with
```scala
val x$ = Ref[T](init)
def x = x$.get
mut def x_=(y: T) = x$.put(y)
```

There should be a way to exclude a mutable variable or field from tracking. Maybe an annotation or modifier such as `transparent` or `untracked`?

The expansion outlined above justifies the following rules for handling mutable variables directly:

 - A type with non-private tracked mutable fields is classified as mutable.
   It has to extend the `Mutable` class.
 - A read access to a local mutable variable `x` charges the capability `x.rd` to the environment.
 - An assignment to a local mutable variable `x` charges the capability `x` to the environment.
 - A read access to a mutable field `this.x` charges the capability `this.rd` to the environment.
 - A write access to a mutable field `this.x` charges the capability `this` to the environment.

Mutable Scopes
==============

We sometimes want to make separation checking coarser. For instance when constructing a doubly linked list we want to create `Mutable` objects and
store them in mutable variables. Since a variable's type cannot contain `cap`,
we must know beforehand what mutable objects it can be refer to. This is impossible if the other objects are created later.

Mutable scopes provide a solution to this they permit to derive a set of variables from a common exclusive reference. We define a new class
```scala
class MutableScope extends Mutable
```
To make mutable scopes useful, we need a small tweak
of the rule governing `new` in the _Mutable Types_ section. The previous rule was:

> When we create an instance of a mutable type we always add `cap` to its capture set.

The new rule is:

> When we create an instance of a mutable type we search for a given value of type `MutableScope`. If such a value is found (say it is `ms`) then we use
`ms` as the capture set of the created instance. Otherwise we use `cap`.

We could envisage using mutable scopes like this:
```
object enclave:
  private given ms: MutableScope()

  ...
```
Within `enclave` all mutable objects have `ms` as their capture set. So they can contain variables that also have `ms` as their capture set of their values.

Mutable scopes should count as mutable types (this can be done either by decree or by adding an update method to `MutableScope`). Hence, mutable scopes can themselves be nested inside other mutable scopes.

## Consumed Capabilities

We allow `consume` as a modifier on parameters and methods. Example:

```scala
class C extends Capability

class Channel[T]:
  def send(consume x: T)



class Buffer[+T] extends Mutable:
  consume def append(x: T): Buffer[T]^

b.append(x)
b1.append(y)

def concat[T](consume buf1: Buffer[T]^, buf2: Buffer[T]): Buffer[T]^

A ->{x.consume} B


A

  C , Gamma, x: S |- t; T
  ---------------------------
  , Gamma |- (x -> t): S ->C T


  C, Gamma |- let x = s in t: T


class Iterator[T]:
  consume def filter(p: T => Boolean): Iterator[T]^
  consume def exists(p: T => Boolean): Boolean
```

As a parameter, `consume` implies `^` as capture set of the parameter type. The `^` can be given, but is redundant.

When a method with a `consume` parameter of type `T2^` is called with an argument of type `T1`, we add the elements of `tcs(hidden(T1, T2^))` not just to the enclosing shadowed set  but to all enclosing shadowed sets where elements are visible. This makes these elements permanently inaccessible.



val f = Future { ... }
val g = Future { ... }


A parameter is implicitly @unbox if it contains a boxed cap. Example:

def apply[T](f: Box[T => T], y: T): T =
  xs.head(y)

def compose[T](fs: @unbox List[T => T]) =
  xs.foldRight(identity)((f: T => T, g: T => T) => x => g(f(x)))



compose(List(f, g))

f :: g :: Nil

def compose[T](fs: List[Unbox[T => T]], x: T) =
  val combined = (xs.foldRight(identity)((f: T => T, g: T => T) => x => g(f(x)))): T->{fs*} T
  combined(x)


With explicit
