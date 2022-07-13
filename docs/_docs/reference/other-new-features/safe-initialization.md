---
layout: doc-page
title: "Safe Initialization"
nightlyOf: https://docs.scala-lang.org/scala3/reference/other-new-features/safe-initialization.html
---

Scala 3 implements experimental safe initialization check, which can be enabled by the compiler option `-Ysafe-init`.

The design and implementation of the initialization checker is described in the
paper _Safe object initialization, abstractly_ [3].

## A Quick Glance

To get a feel of how it works, we first show several examples below.

### Parent-Child Interaction

Given the following code snippet:

``` scala
abstract class AbstractFile:
  def name: String
  val extension: String = name.substring(4)

class RemoteFile(url: String) extends AbstractFile:
  val localFile: String = s"${url.##}.tmp"  // error: usage of `localFile` before it's initialized
  def name: String = localFile
```

The checker will report:

``` scala
-- Warning: tests/init/neg/AbstractFile.scala:7:4 ------------------------------
7 |	val localFile: String = s"${url.##}.tmp"  // error: usage of `localFile` before it's initialized
  |	    ^
  |    Access non-initialized field value localFile. Calling trace:
  |     -> val extension: String = name.substring(4)	[ AbstractFile.scala:3 ]
  |      -> def name: String = localFile            	[ AbstractFile.scala:8 ]
```

### Inner-Outer Interaction

Given the code below:

``` scala
object Trees:
  class ValDef { counter += 1 }
  class EmptyValDef extends ValDef
  val theEmptyValDef = new EmptyValDef
  private var counter = 0  // error
```

The checker will report:

``` scala
-- Warning: tests/init/neg/trees.scala:5:14 ------------------------------------
5 |  private var counter = 0  // error
  |              ^
  |             Access non-initialized field variable counter. Calling trace:
  |              -> val theEmptyValDef = new EmptyValDef    [ trees.scala:4 ]
  |               -> class EmptyValDef extends ValDef       [ trees.scala:3 ]
  |                -> class ValDef { counter += 1 }	     [ trees.scala:2 ]
```

### Functions

Given the code below:

``` scala
abstract class Parent:
  val f: () => String = () => this.message
  def message: String

class Child extends Parent:
  val a = f()
  val b = "hello"           // error
  def message: String = b
```

The checker reports:

``` scala
-- Warning: tests/init/neg/features-high-order.scala:7:6 -----------------------
7 |  val b = "hello"           // error
  |      ^
  |Access non-initialized field value b. Calling trace:
  | -> val a = f()                              	[ features-high-order.scala:6 ]
  |   -> val f: () => String = () => this.message	[ features-high-order.scala:2 ]
  |    -> def message: String = b	                [ features-high-order.scala:8 ]
```
## Design Goals

We establish the following design goals:

- __Sound__: checking always terminates, and is sound for common and reasonable usage (over-approximation)
- __Expressive__: support common and reasonable initialization patterns
- __Friendly__: simple rules, minimal syntactic overhead, informative error messages
- __Modular__: modular checking, no analysis beyond project boundary
- __Fast__: instant feedback
- __Simple__: no changes to core type system, explainable by a simple theory

By _reasonable usage_, we include the following use cases (but not restricted to them):

- Access fields on `this` and outer `this` during initialization
- Call methods on `this` and outer `this` during initialization
- Instantiate inner class and call methods on such instances during initialization
- Capture fields in functions

## Principles

To achieve the goals, we uphold the following fundamental principles:
_stackability_, _monotonicity_, _scopability_ and _authority_.

Stackability means that all fields of a class are initialized at the end of the
class body. Scala enforces this property in syntax by demanding that all fields
are initialized at the end of the primary constructor, except for the language
feature below:

``` scala
var x: T = _
```

Control effects such as exceptions may break this property, as the
following example shows:

``` scala
class MyException(val b: B) extends Exception("")
class A:
  val b = try { new B } catch { case myEx: MyException => myEx.b }
  println(b.a)

class B:
  throw new MyException(this)
  val a: Int = 1
```

In the code above, the control effect teleport the uninitialized value
wrapped in an exception. In the implementation, we avoid the problem
by ensuring that the values that are thrown must be transitively initialized.

Monotonicity means that the initialization status of an object should
not go backward: initialized fields continue to be initialized, a
field points to an initialized object may not later point to an
object under initialization. As an example, the following code will be rejected:

``` scala
trait Reporter:
  def report(msg: String): Unit

class FileReporter(ctx: Context) extends Reporter:
  ctx.typer.reporter = this                // ctx now reaches an uninitialized object
  val file: File = new File("report.txt")
  def report(msg: String) = file.write(msg)
```

In the code above, suppose `ctx` points to a transitively initialized
object. Now the assignment at line 3 makes `this`, which is not fully
initialized, reachable from `ctx`. This makes field usage dangerous,
as it may indirectly reach uninitialized fields.

Monotonicity is based on a well-known technique called _heap monotonic
typestate_ to ensure soundness in the presence of aliasing
[1]. Roughly speaking, it means initialization state should not go backwards.

Scopability means that there are no side channels to access to partially
constructed objects. Control effects like coroutines, delimited
control, resumable exceptions may break the property, as they can transport a
value upper in the stack (not in scope) to be reachable from the current scope.
Static fields can also serve as a teleport thus breaks this property.  In the
implementation, we need to enforce that teleported values are transitively
initialized.

The three principles above contribute to _local reasoning about initialization_,
which means:

> An initialized environment can only produce initialized values.

For example, if the arguments to an `new`-expression are transitively
initialized, so is the result. If the receiver and arguments in a method call
are transitively initialized, so is the result.

Local reasoning about initialization gives rise to a fast initialization
checker, as it avoids whole-program analysis.

The principle of authority goes hand-in-hand with monotonicity: the principle
of monotonicity stipulates that initialization states cannot go backwards, while
the principle of authority stipulates that the initialization states may not
go forward at arbitrary locations due to aliasing. In Scala, we may only
advance initialization states of objects in the class body when a field is
defined with a mandatory initializer or at local reasoning points when the object
becomes transitively initialized.

## Abstract Values

There are three fundamental abstractions for initialization states of objects:

- __Cold__: A cold object may have uninitialized fields.
- __Warm__: A warm object has all its fields initialized but may reach _cold_ objects.
- __Hot__: A hot object is transitively initialized, i.e., it only reaches warm objects.

In the initialization checker, the abstraction `Warm` is refined to handle inner
classes and multiple constructors:

- __Warm[C] { outer = V, ctor, args = Vs }__: A warm object of class `C`, where the immediate outer of `C` is `V`, the constructor is `ctor` and constructor arguments are `Vs`.

The initialization checker checks each concrete class separately. The abstraction `ThisRef`
represents the current object under initialization:

- __ThisRef[C]__: The current object of class `C` under initialization.

The initialization state of the current object is stored in the abstract heap as an
abstract object. The abstract heap also serves as a cache for the field values
of warm objects. `Warm` and `ThisRef` are "addresses" of the abstract objects stored
in the abstract heap.

Two more abstractions are introduced to support functions and conditional
expressions:

- __Fun(e, V, C)__: An abstract function value where `e` is the code, `V` is the
  abstract value for `this` inside the function body and the function is located
  inside the class `C`.

- __Refset(Vs)__: A set of abstract values `Vs`.

A value `v` is _effectively hot_ if any of the following is true:

- `v` is `Hot`.
- `v` is `ThisRef` and all fields of the underlying object are assigned.
- `v` is `Warm[C] { ... }` and
  1. `C` does not contain inner classes; and
  2. Calling any method on `v` encounters no initialization errors and the method return value is _effectively hot_; and
  3. Each field of `v` is _effectively hot_.
- `v` is `Fun(e, V, C)` and calling the function encounters no errors and the
  function return value is _effectively hot_.
- The root object (refered by `ThisRef`) is _effectively hot_.

An effectively hot value can be regarded as transitively initialized thus can
be safely leaked via method arguments or as RHS of an reassignment.
The initialization checker tries to promote non-hot values to effectively hot
whenenver possible.

## Rules

With the established principles and design goals, the following rules are imposed:

1. The field access `e.f` or method call `e.m()` is illegal if `e` is _cold_.

   A cold value should not be used.

2. The field access `e.f` is invalid if `e` has the value `ThisRef` and `f` is not initialized.

3. In an assignment `o.x = e`, the expression `e` must be _effectively hot_.

   This is how monotonicity is enforced in the system. Note that in an
   initialization `val f: T = e`, the expression `e` may point to a non-hot
   value.

4. Arguments to method calls must be _effectively hot_.

   Escape of `this` in the constructor is commonly regarded as an anti-pattern.

   However, passing non-hot values as argument to another constructor is allowed, to support
   creation of cyclic data structures. The checker will ensure that the escaped
   non-initialized object is not used, i.e. calling methods or accessing fields
   on the escaped object is not allowed.

   An exception is for calling synthetic `apply`s of case classes. For example,
   the method call `Some.apply(e)` will be interpreted as `new Some(e)`, thus
   is valid even if `e` is not hot.

   Another exception to this rule is parametric method calls. For example, in
   `List.apply(e)`, the argument `e` may be non-hot. If that is the case, the
   result value of the parametric method call is taken as _cold_.

5. Method calls on hot values with effectively hot arguments produce hot results.

   This rule is assured by local reasoning about initialization.

6. Method calls on `ThisRef` and warm values will be resolved statically and the
   corresponding method bodies are checked.

7. In a new expression `new p.C(args)`, if the values of `p` and `args` are
   effectively hot, then the result value is also hot.

   This is assured by local reasoning about initialization.

8. In a new expression `new p.C(args)`, if any value of `p` and `args` is not
   effectively hot, then the result value takes the form `Warm[C] { outer = Vp, args = Vargs }`. The initialization code for the class `C` is checked again to make
   sure the non-hot values are used properly.

   In the above, `Vp` is the widened value of `p` --- the widening happens if `p`
   is a warm value `Warm[D] { outer = V, args }` and we widen it to
   `Warm[D] { outer = Cold, args }`.

   The variable `Vargs` represents values of `args` with non-hot values widened
   to `Cold`.

   The motivation for the widening is to finitize the abstract domain and ensure
   termination of the initialization check.

9. The scrutinee in a pattern match and the values in return and throw statements must be _effectively hot_.

## Modularity

The analysis takes the primary constructor of concrete classes as entry points.
It follows the constructors of super classes, which might be defined in another project.
The analysis takes advantage of TASTy for analyzing super classes defined in another project.

The crossing of project boundary raises a concern about modularity. It is
well-known in object-oriented programming that superclass and subclass are
tightly coupled. For example, adding a method in the superclass requires
recompiling the child class for checking safe overriding.

Initialization is no exception in this respect. The initialization of an object
essentially involves close interaction between subclass and superclass. If the
superclass is defined in another project, the crossing of project boundary
cannot be avoided for soundness of the analysis.

Meanwhile, inheritance across project boundary has been under scrutiny and the
introduction of [open classes](./open-classes.md) mitigate the concern here.
For example, the initialization check could enforce that the constructors of
open classes may not contain method calls on `this` or introduce annotations as
a contract.

The feedback from the community on the topic is welcome.

## Back Doors

Occasionally you may want to suppress warnings reported by the
checker.  You can either write `e: @unchecked` to tell the checker to
skip checking for the expression `e`, or you may use the old trick:
mark some fields as lazy.

## Caveats

- The system cannot provide safety guarantee when extending Java or Scala 2 classes.
- Safe initialization of global objects is only partially checked.

## References

1. Fähndrich, M. and Leino, K.R.M., 2003, July. [_Heap monotonic typestates_](https://www.microsoft.com/en-us/research/publication/heap-monotonic-typestate/). In International Workshop on Aliasing, Confinement and Ownership in object-oriented programming (IWACO).
2. Fengyun Liu, Ondřej Lhoták, Aggelos Biboudis, Paolo G. Giarrusso, and Martin Odersky. [_A type-and-effect system for object initialization_](https://dl.acm.org/doi/10.1145/3428243). OOPSLA, 2020.
3. Fengyun Liu, Ondřej Lhoták, Enze Xing, Nguyen Cao Pham. [_Safe object initialization, abstractly_](https://dl.acm.org/doi/10.1145/3486610.3486895). Scala 2021.
