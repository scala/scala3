---
layout: doc-page
title: "Erased Definitions"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/erased-defs.html
---

`erased` is a modifier that expresses that some value or parameter is erased by the compiler instead of being represented in the compiled output. It is not yet part of the Scala language standard. To enable `erased`, turn on the language feature
[`experimental.erasedDefinitions`](https://scala-lang.org/api/3.x/scala/runtime/stdLibPatches/language$$experimental$$erasedDefinitions$.html). This can be done with a language import
```scala
import scala.language.experimental.erasedDefinitions
```
or by setting the command line option `-language:experimental.erasedDefinitions`.

## Introduction

## Why erased?

Sometimes, we need a value only to present evidence that some type can be constructed, whereas at runtime that value would not be referenced. For example, say we want to make Java serialization safe. This means that, when serializing values of some type, we want to have evidence that serialization of such a type will not fail at runtime. Java defines the `java.io.Serializable` interface to mark extending types as serializable. But this alone is not safe, since a `Serializable` class might well have unserializable fields. For instance Scala's `List` extends `Serializable` since we want to be able to serialize `List` data. But a particular list might have elements that are not serializable, for instance it might be a list of functions. If we try to serialize such a value, a `NotSerializableException` will be thrown.

We can make serialization safe by defining an additional type class that has instances precisely for those types that are deeply serializable. For instance, like this:
```scala
/** Type class for types that are deeply serializable */
trait CanSerialize[T]

inline given CanSerialize[String] = CanSerialize()
inline given [T: CanSerialize] => CanSerialize[List[T]] = CanSerialize()
```
We find a given instance of `CanSerialize` for strings, since strings are serializable. We also find a conditional given instance that says lists are serializable if their elements are. We would assume to have further instances for all types that are serializable (perhaps conditionally).

Now, we can formulate a method `safeWriteObject` that serializes an object to an `ObjectOutputStream`:
```scala
def safeWriteObject[T <: java.io.Serializable]
    (out: java.io.ObjectOutputStream, x: T)
    (using CanSerialize[T]): Unit =
  out.writeObject(x)
```
The method works for objects of its type parameter `T`. `T` is required to conform to `java.io.Serializable` so that we can use the `writeObject` method of the output stream `out` on it. In addition we need a type class instance `CanSerialize[T]` that serves as evidence that the Java serialization will not fail at runtime. We can specialize the method to list arguments, as in the following:
```scala
def writeList[T]
    (out: java.io.ObjectOutputStream, xs: List[T])
    (using CanSerialize[T]): Unit =
  safeWriteObject(out, xs)
```
We can test `writeList` by applying it to different types of lists:
```scala
@main def Test(out: java.io.ObjectOutputStream) =
  writeList(out, List("a", "b"))                            // ok
  writeList(out, List[Int => Int](x => x + 1, y => y * 2))  // error
```
The first call will pass, but the second call will be rejected with a type error:
```
No given instance of type CanSerialize[Int => Int] was found for parameter x$3 of method writeList
```

So far, this is a standard typeclass pattern to set up evidence that certain operations can be performed safely. But there is a problem with this scheme: The type class instances are passed as
additional parameters to methods `safeWriteObject` and `writeList` even though at run-time these objects will not be used anywhere. The only role of these parameters is to provide compile-time evidence that serialization for a particular type is safe. It would be nice if we could somehow "erase" these parameters so that they do not show up at run-time. This is precisely what erased does. Using erased, our example would look like this:
```scala
import language.experimental.erasedDefinitions

class CanSerialize[T]

inline given CanSerialize[String] = CanSerialize()
inline given [T: CanSerialize] => CanSerialize[List[T]] = CanSerialize()

def safeWriteObject[T <: java.io.Serializable](out: java.io.ObjectOutputStream, x: T)(using erased CanSerialize[T]) =
  out.writeObject(x)

def writeList[T](out: java.io.ObjectOutputStream, xs: List[T])(using erased CanSerialize[T]) =
  safeWriteObject(out, xs)

@main def Test(out: java.io.ObjectOutputStream) =
  writeList(out, List("a", "b"))                            // ok
  writeList(out, List[Int => Int](x => x + 1, y => y * 2))  // error
```
Note the two parameters to `safeWriteObject` and `writeList` are now `erased`. This means the parameters and their arguments are not present in the generated code.

A safety requirement for `erased` is that we cannot simply make up evidence. For instance, say we want to make the second `writeList` pass by making up a given of the problematic type:
```scala
writeList(out, List[Int => Int](x => x + 1, y => y * 2))
  (using null.asInstanceOfCanSerialize[Int => Int])
```
This is just one way to do it, here is another:
```scala
def fakeEvidence: CanSerialize[Int => Int] = fakeEvidence
writeList(out, List[Int => Int](x => x + 1, y => y * 2))
  (using fakeEvidence)
```
To rule out these attacks, we demand that the argument to an erased parameter is
a _pure expression_. Only a few expressions in Scala are pure, including

 - constants,
 - non-lazy, immutable vals,
 - constructors of classes that don't have an initializer, applied to pure arguments,
 - `apply` methods of case classes that don't have an initializer, applied to pure arguments.

Other function calls are not classified as pure expressions. That's why the two given instances in the erased version of our examples are inline methods. After inlining, the arguments to the erased parameters are simple class constructions of `CanSerialize` which count as pure expressions.

## Details

Parameters of methods and functions can be declared as erased, placing `erased`
in front of each erased parameter (like `inline`).

```scala
def methodWithErasedEv(erased ev: Ev, x: Int): Int = x + 2

val lambdaWithErasedEv: (erased Ev, Int) => Int =
  (erased ev, x) => x + 2
```

`erased` parameters will not be usable for computations, though they can be used
as arguments to other `erased` parameters.

```scala
def methodWithErasedInt1(erased i: Int): Int =
  i + 42 // ERROR: can not use i

def methodWithErasedInt2(erased i: Int): Int =
  methodWithErasedInt1(i) // OK
```

The arguments to erased parameters must be pure expressions.
```scala
def f(x: Int): Int =
  if x == 0 then 1 else x * f(x - 1)

inline def g(x: Int): Int =
  if x == 0 then 1 else x * g(x - 1)

methodWithErasedInt2(5)  // ok
methodWithErasedInt2(f(5))  // error, f(22) is not a pure expression
methodWithErasedInt2(g(5))  // ok since `g` is `inline`.

Besides parameters, `val` definitions can also be marked with `erased`.
These will also only be usable as arguments to `erased` parameters or
as part of the definitions of other erased `val`s. Furthermore, the
defining right hand side of such `val` must be a pure expression.

```scala
erased val erasedEvidence: Ev = Ev()
methodWithErasedEv(erasedEvidence, 40) // 42
```

## The Erased Trait

In some cases we would expect all instances of a trait to be erased. For instance, one could argue that it does not make sense to ever have a `CanSerialize[T]` instance at runtime. In that case we
can make `CanSerialize` extend from a new trait `compiletimetime.Erased` and avoid the explicit
`erased` modifiers in erased parameters and vals. Here is an alternative version of our example using this scheme:
```scala
class CanSerialize[T] extends compiletime.Erased
...
def safeWriteObject[T <: java.io.Serializable](out: java.io.ObjectOutputStream, x: T)(using CanSerialize[T]) = ...

def writeList[T: CanSerialize](out: java.io.ObjectOutputStream, xs: List[T]) = ...
```
Because `CanSerialize` extends `Erased` we can elide the explicit `erased` modifier in the using clause of `safeWriteObject`. It now also becomes possible to use a context bound for `CanSerialize` as is shown in the `writeList` method above. The context bound expands to a
using clause `(using CanSerialize[T])` which gets implicitly tagged with `erased`.

## Uses of `Erased` in existing Code

 - The `CanThrow[T]` type class is used to declare that an exception can be thrown. The compiler generates a  `CanThrow[E]` instances for exceptions that are handled in a `try`. Methods take an implicit `CanThrow[E]` parameter to indicate that they might throw exception `E`. `CanThrow` is declared to be an `Erased` capability class, so no actual evidence of `CanThrow` remains at run-time.

 - The `CanEqual` evidence of [multiversal equality](../contextual/multiversal-equality.html) checks that two types can be compared. The actual comparison is done by the universal `equals` method of class `Object` or an overriding instance, it does not rely on the `CanEqual` value.
So far, `CanEqual` is handled specially in the compiler. With erased definitions, we could
avoid some of the special treatment by making `CanEqual` extend `compiletime.Erased`.

- The conforms `<:<` typeclass asserts that we can prove that two types are in a subtype relation. `<:<` does offer a method to upcast values, but that could be also provided as a compiler-generated
cast operation. In that case, run-time instances of `<:<` (and also `=:=`) would be no longer needed and could be erased.


## Example: State machine with erased evidence

The following example is an extended implementation of a simple state machine
which can be in a state `On` or `Off`. The machine can change state from `Off`
to `On` with `turnOn` only if it is currently `Off`, conversely from `On` to
`Off` with `turnOff` only if it is currently `On`. These constraints are
captured represented with two typeclass traits `IsOn[T]` and `IsOff[T]`. Two given instances for these traits exist only for the right kinds of state. There is a given instance for `IsOn[On]` and one for `IsOff[Off]` but there are no given instances for the other combinations.

The `turnOn` and `turnOff` methods each require one of these given instances to ensure the machine is in the correct state for the operation to be allowed.
As the given instances required by `turnedOn` and `turnedOff` are not used in the bodies of those functions we can mark them as `erased`.

```scala
import language.experimental.erasedDefinitions
import scala.annotation.implicitNotFound

sealed trait State
final class On extends State
final class Off extends State

@implicitNotFound("State must be Off")
class IsOff[S <: State]
object IsOff:
  inline given IsOff[Off]()

@implicitNotFound("State must be On")
class IsOn[S <: State]
object IsOn:
  inline given IsOn[On]()

class Machine[S <: State]:
  // ev will disappear from both functions
  def turnOn(using erased IsOff[S]): Machine[On] = new Machine[On]
  def turnOff(using erased IsOn[S]): Machine[Off] = new Machine[Off]

@main def test =
  val m = Machine[Off]()
  val m1 = m.turnOn
  val m2 = m1.turnOff
  m2.turnOn

  // m1.turnOn
  //          ^ error: State must be Off
  // m2.turnOff
  //           ^ error: State must be On
```
The first four lines of method `test` are all valid. The commented-out operations are invalid. The operation `m1.turnOn` is invalid since `m1` is of type `Machine[On]` and `m1.turnOn` requires the given instance `IsOff[On]` which does not exist. `m2.turnOff` is invalid by analogous reasoning.

## ErasedValue

The `compiletime.erasedValue` method was discussed in
[Compile-time operations](../metaprogramming/compiletime-ops.md#erasedvalue). A call to `erasedValue[T]` counts as an erased reference, so it could only be
used in an erased context, i.e. as an argument to an erased parameter or on the right-hand side of an erased `val` definition. At the same time
`erasedValue` does _not_ count as a pure expression, and for that reason cannot be part of these expressions. The net effect is that any references
to `erasedValue` must be eliminated by inlining. This is intentional:
allowing `erasedValue[T]` as a legal erased expression would undermine the safety of erased capabilities, since evidence for _any_ value of an erased type can be made up by it.

As an escape hatch, there also a method `unsafeErasedValue` in the
`scala.caps.unsafe` object. `scala.caps.unsafe.unsafeErasedValue[T]` does count as a pure expression for every type `T`, so it can be used in an erased context. But it should be used only if we can prove by other means that the established erased evidence is valid.

[More Details](./erased-defs-spec.md)
