---
layout: doc-page
title: "Erased Terms"
---

# Why erased terms?

Let's describe the motivation behind erased terms with an example. In the
following we show a simple state machine which can be in a state `On` or `Off`.
The machine can change state from `Off` to `On` with `turnedOn` only if it is
currently `Off`. This last constraint is captured with the `IsOff[S]` implicit
evidence which only exists for `IsOff[Off]`. For example, not allowing calling
`turnedOn` on in an `On` state as we would require an evidence of type
`IsOff[On]` that will not be found.

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

Note that in the code above the actual implicit arguments for `IsOff` are never
used at runtime; they serve only to establish the right constraints at compile
time. As these terms are never used at runtime there is not real need to have
them around, but they still need to be present in some form in the generated
code to be able to do separate compilation and retain binary compatibility. We
introduce _erased terms_ to overcome this limitation: we are able to enforce the
right constrains on terms at compile time. These terms have no run time
semantics and they are completely erased.

# How to define erased terms?

Parameters of methods and functions can be declared as erased, placing `erased`
in front of a parameter list (like `given`).

```scala
def methodWithErasedEv erased (ev: Ev): Int = 42

val lambdaWithErasedEv: erased Ev => Int =
  erased (ev: Ev) => 42
```

`erased` parameters will not be usable for computations, though they can be used
as arguments to other `erased` parameters.

```scala
def methodWithErasedInt1 erased (i: Int): Int =
  i + 42 // ERROR: can not use i

def methodWithErasedInt2 erased (i: Int): Int =
  methodWithErasedInt1(i) // OK
```

Not only parameters can be marked as erased, `val` and `def` can also be marked
with `erased`. These will also only be usable as arguments to `erased`
parameters.

```scala
erased val erasedEvidence: Ev = ...
methodWithErasedEv(erasedEvidence)
```

# What happens with erased values at runtime?

As `erased` are guaranteed not to be used in computations, they can and will be
erased.

```scala
// becomes def methodWithErasedEv(): Int at runtime
def methodWithErasedEv erased (ev: Ev): Int = ...

def evidence1: Ev = ...
erased def erasedEvidence2: Ev = ... // does not exist at runtime
erased val erasedEvidence3: Ev = ... // does not exist at runtime

// evidence1 is not evaluated and no value is passed to methodWithErasedEv
methodWithErasedEv(evidence1)
```

# State machine with erased evidence example

The following example is an extended implementation of a simple state machine
which can be in a state `On` or `Off`. The machine can change state from `Off`
to `On` with `turnedOn` only if it is currently `Off`, conversely from `On` to
`Off` with `turnedOff` only if it is currently `On`. These last constraint are
captured with the `IsOff[S]` and `IsOn[S]` implicit evidence only exist for
`IsOff[Off]` and `IsOn[On]`. For example, not allowing calling `turnedOff` on in
an `Off` state as we would require an evidence `IsOn[Off]` that will not be
found.

As the implicit evidences of `turnedOn` and `turnedOff` are not used in the
bodies of those functions we can mark them as `erased`. This will remove the
evidence parameters at runtime, but we would still evaluate the `isOn` and
`isOff` implicits that were found as arguments. As `isOn` and `isOff` are not
used except as `erased` arguments, we can mark them as `erased`, hence removing
the evaluation of the `isOn` and `isOff` evidences.

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
  // erased val isOn will not exist at runtime, the compiler will only require that this evidence exists at compile time
  erased implicit val isOn: IsOn[On] = new IsOn[On]
}

class Machine[S <: State] private {
  // ev will disappear from both functions
  def turnedOn given erased (ev: IsOff[S]): Machine[On] = new Machine[On]
  def turnedOff given erased (ev: IsOn[S]): Machine[Off] = new Machine[Off]
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

Note that in [Inline](./inline.html) we discussed `erasedValue` and inline
matches. `erasedValue` is implemented with `erased`, so the state machine above
can be encoded as follows:

```scala
import scala.compiletime._

sealed trait State
final class On extends State
final class Off extends State

class Machine[S <: State] {
  inline def turnOn() <: Machine[On] = inline erasedValue[S] match {
    case _: Off  => new Machine[On]
    case _: On   => error("Turning on an already turned on machine")
  }
  inline def turnOff() <: Machine[Off] = inline erasedValue[S] match {
    case _: On  => new Machine[Off]
    case _: Off   => error("Turning off an already turned off machine")
  }
}

object Machine {
  def newMachine(): Machine[Off] = {
    println("newMachine")
    new Machine[Off]
  }
}

object Test {
  val m = Machine.newMachine()
  m.turnOn()
  m.turnOn().turnOff()
  m.turnOn().turnOn() // error: Turning on an already turned on machine
}
```

[More Details](./erased-terms-spec.html)