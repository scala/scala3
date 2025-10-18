---
layout: doc-page
title: "Modularity Improvements"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/modularity.html
---

# Modularity Improvements

Martin Odersky, 7.1.2024

Scala is a language in the SML tradition, in the sense that it has
abstract and alias types as members of modules (which in Scala take the form of objects and classes). This leads to a simple dependently
typed system, where dependencies in types are on paths instead of full terms.

So far, some key ingredients were lacking which meant that module composition with functors is harder in Scala than in SML. In particular, one often needs to resort the infamous `Aux` pattern that lifts type members into type parameters so that they can be tracked across class instantiations. This makes modular, dependently typed programs
much harder to write and read, and makes such programming only accessible to experts.

In this note I propose some small changes to Scala's dependent typing that makes
modular programming much more straightforward.

The suggested improvements have been implemented and are available
in source version `future` if the additional experimental language import `modularity` is present. For instance, using the following command:

```
  scala compile -source:future -language:experimental.modularity
```

## Tracked Parameters

Scala is dependently typed for functions, but unfortunately not for classes.
For instance, consider the following definitions:

```scala
  class C:
    type T
    ...

  def f(x: C): x.T = ...

  val y: C { type T = Int }
```
Then `f(y)` would have type `Int`, since the compiler will substitute the
concrete parameter reference `y` for the formal parameter `x` in the result
type of `f`, and `y.T = Int`

However, if we use a class `F` instead of a method `f`, things go wrong.

```scala
  class F(val x: C):
    val result: x.T = ...
```
Now `F(y).result` would not have type `Int` but instead the rather less useful type `?1.T` where `?1` is a so-called skolem constant of type `C` (a skolem represents an unknown value).

This shortcoming means that classes cannot really be used for advanced
modularity constructs that rely on dependent typing.

**Proposal:** Introduce a `tracked` modifier that can be added to
a `val` parameter of a class or trait. For every tracked class parameter of a class `C`, add a refinement in the constructor type of `C` that the class member is the same as the parameter.

**Example:** In the setting above, assume `F` is instead declared like this:
```scala
  class F(tracked val x: C):
    val result: x.T = ...
```
Then the constructor `F` would get roughly the following type:
```scala
  F(x1: C): F { val x: x1.type }
```
_Aside:_ More precisely, both parameter and refinement would apply to the same name `x` but the refinement still refers to the parameter. We unfortunately can't express that in source, however, so we chose the new name `x1` for the parameter in the explanation.

With the new constructor type, the expression `F(y).result` would now have the type `Int`, as hoped for. The reasoning to get there is as follows:

 - The result of the constructor `F(y)` has type `F { val x: y.type }` by
   the standard typing for dependent functions.
 - The type of `result` inside `F` is `x.T`.
 - Hence, the type of `result` as a member of `F { val x: y.type }` is `y.T`, which is equal to `Int`.

The addition of tracked parameters makes classes suitable as a fundamental modularity construct supporting dependent typing. Here is an example, taken from issue #3920:

```scala
trait Ordering:
  type T
  def compare(t1:T, t2: T): Int

class SetFunctor(tracked val ord: Ordering):
  type Set = List[ord.T]

  def empty: Set = Nil

  extension (s: Set)
    def add(x: ord.T): Set = x :: remove(x)
    def remove(x: ord.T): Set = s.filter(e => ord.compare(x, e) != 0)
    def contains(x: ord.T): Boolean = s.exists(e => ord.compare(x, e) == 0)

object intOrdering extends Ordering:
  type T = Int
  def compare(t1: T, t2: T): Int = t1 - t2

val IntSet = new SetFunctor(intOrdering)

@main def Test =
  import IntSet.*
  val set = IntSet.empty.add(6).add(8).add(23)
  assert(!set.contains(7))
  assert(set.contains(8))
```
This works as it should now. Without the addition of `tracked` to the
parameter of `SetFunctor` typechecking would immediately lose track of
the element type `T` after an `add`, and would therefore fail.

**Syntax Change**

```
ClsParam  ::=  {Annotation} [{Modifier | ‘tracked’} (‘val’ | ‘var’)] Param
```

The (soft) `tracked` modifier is only allowed for `val` parameters of classes.

### Tracked Inference

In some common cases the tracked modifier can be inferred, so it does not
need to be written explicitly. Specifically, we infer `tracked` for a `val`
parameter of a class if the formal parameter's type defines an abstract type member.
This means that we do not lose information about how that member
is defined in the actual argument passed to the class constructor.

For instance, tracked `would` be inferred for the `SetFunctor` class
we defined before, so we can also write it like this:
```scala
class SetFunctor(val ord: Ordering):
  type Set = List[ord.T]
  ...
```
The `tracked` modifier on the `ord` parameter is inferred here, since `ord` is of type `Ordering`, which defines an abstract type member `T`.

Another common case is when a context bound has an associated type (i.e. an abstract type member) e.g.
```scala 3
trait TC:
  type Self
  type T

class Klass[A: {TC as tc}]
```

Here, `tc` is a context bound with an associated type `T`, so `tracked val` will be inferred for `tc` and the parameter will be represented as a field.

### Discussion

Since `tracked` is so useful, why not assume it by default? First, `tracked` makes sense only for `val` parameters. If a class parameter is not also a field declared using `val` then there's nothing to refine in the constructor result type. One could think of at least making all `val` parameters tracked by default, but that would be a backwards incompatible change. For instance, the following code would break:

```scala
case class Foo(x: Int)
var foo = Foo(1)
if someCondition then foo = Foo(2)
```
If we assume `tracked` for parameter `x` (which is implicitly a `val`),
then `foo` would get inferred type `Foo { val x: 1 }`, so it could not
be reassigned to a value of type `Foo { val x: 2 }` on the next line.

Another concern is that using tracked for all `val` parameters, including
parameters of case classes could lead to large refinement types.

Therefore, inferring tracked only for parameters with types that define abstract members is a usable compromise. After all, if we did not infer `tracked` for these types, any references to the abstract type via a path would likely produce compilation errors.

## Tracked members

The `tracked` modifier can also be used for `val` members of classes and traits
to force the type of the member (or it's overriding member) to be as exact as
possible. More precisely, it will will assign the `tracked` member the infered
type of the rhs. For instance, consider the following definition:

```scala
trait F:
  tracked val a: Int
  tracked val b: Int

class N extends F:
  val a = 22 // a.type =:= 22
  val b: Int = 22 // b.type =:= Int
  tracked val c = 22 // c.type =:= 22
```

Here, the `tracked` modifier ensures that the type of `a` in `N` is `22` and not
`Int`. But the type of `b` is `N` is `Int` since it's explicitly declared as
`Int`. `tracked` members can also be immediately initialized, as in the case of
`c`.

## Tracked syntax change

```
LocalModifier     ::=  ‘tracked’
```

The (soft) `tracked` modifier is allowed as a local modifier.

## Applied constructor types

A new syntax is also introduced, to make classes with `tracked` parameters
easier to use. The new syntax is essentially the ability to use an application
of a class constructor as a type, we call such types applied constructor types.

With this new feature the following example compiles correctly and the type in
the comment is the resulting type of the applied constructor types.

```scala
import scala.language.experimental.modularity

class C(tracked val v: Any)

val c: C(42) /* C { val v: 42 } */ = C(42)
```

### Syntax change

```
SimpleType        ::=  SimpleLiteral
                    |  ‘?’ TypeBounds
---                 |  SimpleType1
+++                 |  SimpleType1 {ParArgumentExprs}
```

A `SimpleType` can now optionally be followed by `ParArgumentExprs`.

The arguments are used to typecheck the whole type, as if it was a normal
constructor application. For classes with `tracked` parameters this will mean
that the resulting type will have a refinement for each `tracked` parameter.

For example, given the following class definition:
```scala
class Person(tracked val name: String, tracked val age: Int)
```
**Type** `Person("Kasia", 27)` will be translated to `Person { val name: "Kasia"; val age: 27 }`.

## Allow Class Parents to be Refined Types

Since `tracked` parameters create refinements in constructor types,
it is now possible that a class has a parent that is a refined type.
Previously such types were not permitted, since we were not quite sure how to handle them. But with tracked parameters it becomes pressing to
admit such types.

**Proposal** Allow refined types as parent types of classes. All refinements that are inherited in this way become synthetic members of the class.

**Example**

```scala
class C:
  type T
  def m(): T

type R = C:
  type T = Int
  def m(): 22

class D extends R:
  def next(): D
```
This code now compiles. The definition of `D` is expanded as follows:

```scala
class D extends C:
  def next(): D
  /*synthetic*/ type T = Int
  /*synthetic*/ def m(): 22
```
Note how class refinements are moved from the parent constructor of `D` into the body of class `D` itself.

This change does not entail a syntax change. Syntactically, parent types cannot be refined types themselves. So the following would be illegal:
```scala
class D extends C { type T = Int; def m(): 22 }: // error
  def next(): D
```
If a refined type should be used directly as a parent type of a class, it needs to come in parentheses:
```scala
class D extends (C { type T = Int; def m(): 22 }) // ok
  def next(): D
```

## A Small Relaxation To Export Rules

The rules for export forwarders are changed as follows.

Previously, all export forwarders were declared `final`. Now, only term members are declared `final`. Type aliases are left aside.

This makes it possible to export the same type member into several traits and then mix these traits in the same class. The test file `tests/pos/typeclass-aggregates.scala` shows why this is essential if we want to combine multiple givens with type members in a new given that aggregates all these givens in an intersection type.

The change does not lose safety since different type aliases would in any case lead to uninstantiatable classes.