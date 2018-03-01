---
layout: doc-page
title: "Unused Parameters"
---

Why unused parameters?
----------------------
The following examples shows an implementation of a simple state machine which can be in a state `On` or `Off`.
The machine can change state from `Off` to `On` with `turnedOn` only if it is currently `Off`. This last constraint is
captured with the `IsOff[S]` implicit evidence which only exists for `IsOff[Off]`.
For example, not allowing calling `turnedOn` on in an `On` state as we would require an evidence of type `IsOff[On]` that will not be found.

```scala
sealed trait State
final class On extends State
final class Off extends State

@implicitNotFound("State is must be Off")
class IsOff[S <: State]
object IsOff {
  implicit def isOff: IsOff[Off] = new IsOff[Off]
}

class Machine[S <: State] {
  def turnedOn(implicit ev: IsOff[S]): Machine[On] = new Machine[On]
}

val m = new Machine[Off]
m.turnedOn
m.turnedOn.turnedOn // ERROR
//                 ^
//                  State is must be Off
```

Note that in the code above the actual implicit arguments for `IsOff` are never used at runtime; they serve only to establish the right constraints at compile time.
As these parameters are never used at runtime there is not real need to have them around, but they still need to be
present in some form in the generated code to be able to do separate compilation and retain binary compatiblity.

How to define unused parameter?
-------------------------------
Parameters of methods and functions can be declared as unused, placing `unused` at the start of the parameter list (like `implicit`).

```scala
def methodWithUnusedEv(unused ev: Ev): Int = 42

val lambdaWithUnusedEv: unused Ev => Int = 
  unused (ev: Ev) => 42
```

`unused` parameters will not be usable for computations, though they can be used as arguments to other `unused` parameters.

```scala
def methodWithUnusedInt1(unused i: Int): Int =
  i + 42 // ERROR: can not use i

def methodWithUnusedInt2(unused i: Int): Int =
  methodWithUnusedInt1(i) // OK
```

Not only parameters can be marked as unused, `val` and `def` can also be marked with `unused`. These will also only be usable as arguments to `unused` parameters.

```scala
unused val unusedEvidence: Ev = ...
methodWithUnusedEv(unusedEvidence)
```

What happens with unused values at runtime?
-------------------------------------------
As `unused` are guaranteed not to be used in computations, they can and will be erased.

```scala
// becomes def methodWithUnusedEv(): Int at runtime
def methodWithUnusedEv(unused ev: Ev): Int = ...  

def evidence1: Ev = ...
unused def unusedEvidence2: Ev = ... // does not exist at runtime
unused val unusedEvidence3: Ev = ... // does not exist at runtime

// evidence1 is not evaluated and no value is passed to methodWithUnusedEv
methodWithUnusedEv(evidence1)
```

State machine with unused evidence example
------------------------------------------
The following example is an extended implementation of a simple state machine which can be in a state `On` or `Off`.
The machine can change state from `Off` to `On` with `turnedOn` only if it is currently `Off`, 
conversely from `On` to `Off` with `turnedOff` only if it is currently `On`. These last constraint are
captured with the `IsOff[S]` and `IsOn[S]` implicit evidence only exist for `IsOff[Off]` and `InOn[On]`. 
For example, not allowing calling `turnedOff` on in an `Off` state as we would require an evidence `IsOn[Off]` 
that will not be found.

As the implicit evidences of `turnedOn` and `turnedOff` are not used in the bodies of those functions 
we can mark them as `unused`. This will remove the evidence parameters at runtime, but we would still 
evaluate the `isOn` and `isOff` implicits that where found as arguments.
As `isOn` and `isOff` are not used except as as `unused` arguments, we can mark them as `unused`, hence 
removing the evaluation of the `isOn` and `isOff` evidences.

```scala
import scala.annotation.implicitNotFound

sealed trait State
final class On extends State
final class Off extends State

@implicitNotFound("State is must be Off")
class IsOff[S <: State]
object IsOff {
  // def isOff will not be called at runtime for turnedOn, the compiler will only require that this evidence exists
  implicit def isOff: IsOff[Off] = new IsOff[Off]
}

@implicitNotFound("State is must be On")
class IsOn[S <: State]
object IsOn {
  // def isOn will not exist at runtime, the compiler will only require that this evidence exists at compile time
  unused implicit val isOn: IsOn[On] = new IsOn[On]
}

class Machine[S <: State] private {
  // ev will disapear from both functions
  def turnedOn(implicit unused ev: IsOff[S]): Machine[On] = new Machine[On]
  def turnedOff(implicit unused ev: IsOn[S]): Machine[Off] = new Machine[Off]
}

object Machine {
  def newMachine(): Machine[Off] = new Machine[Off]
}

object Test {
  def main(args: Array[String]): Unit = {
    val m = Machine.newMachine()
    m.turnedOn
    m.turnedOn.turnedOff

    // m.turnedOff
    //            ^
    //            State is must be On

    // m.turnedOn.turnedOn
    //                    ^
    //                    State is must be Off
  }
}
```


Rules
-----

1) The `unused` modifier can appear:
* At the start of a parameter block of a method, function or class
* In a method definition
* In a `val` definition (but not `lazy val` or `var`)

```scala
unused val x = ...
unused def f = ...

def g(unused x: Int) = ...

(unused x: Int) => ...
def h(x: unused Int => Int) = ...

class K(unused x: Int) { ... }
```

2) A reference to an `unused` definition can only be used
* Inside the expression of argument to an `unused` parameter
* Inside the body of an `unused` `val` or `def`

3) Functions
* `(unused x1: T1, x2: T2, ..., xN: TN) => y : (unused T1, T2, ..., TN) => R`
* `(implicit unused x1: T1, x2: T2, ..., xN: TN) => y : (implicit unused T1, T2, ..., TN) => R`
* `implicit unused T1 => R  <:<  unused T1 => R`
* `(implicit unused T1, T2) => R  <:<  (unused T1, T2) => R`
*  ...

Note that there is no subtype relation between `unused T => R` and `T => R` (or `implicit unused T => R` and `implicit T => R`)

4) Eta expansion
if `def f(unused x: T): U` then `f: (unused T) => U`.


5) Erasure Semantics
* All `unused` paramters are removed from the function
* All argument to `unused` paramters are not passed to the function
* All `unused` definitions are removed
* All `(unused T1, T2, ..., TN) => R` and `(implicit unused T1, T2, ..., TN) => R` become `() => R`

6) Overloading
Method with `unused` parameters will follow the normal overloading constraints after erasure.

7) Overriding
* Member definitions overidding each other must both be `unused` or not be `unused`
* `def foo(x: T): U` cannot be overriden by `def foo(unused x: T): U` an viceversa

