---
layout: doc-page
title: "Typeable"
---

Typeable
--------

`Typeable` provides the a generalization of `ClassTag.unapply` where the type of the argument is generalized.
`Typeable.unapply` will return `Some(x.asInstanceOf[Y])` if `x` conforms to `Y`, otherwise it returns `None`.

```scala
trait Typeable[S, T <: S] extends Serializable {
  def unapply(x: S): Option[T]
}
```

Just like `ClassTag` it can be used to perform type checks in patterns.

```scala
type X
type Y <: X

given Typeable[X, Y] = ...

(x: X) match {
  case y: Y => ... // safe checked downcast
  case _ => ...
}
```


Examples
--------

Given the following abstract definition of `Peano` numbers that provides `Typeable[Nat, Zero]` and `Typeable[Nat, Succ]`

```scala
trait Peano {
  type Nat
  type Zero <: Nat
  type Succ <: Nat

  def safeDiv(m: Nat, n: Succ): (Nat, Nat)

  val Zero: Zero

  val Succ: SuccExtractor
  trait SuccExtractor {
    def apply(nat: Nat): Succ
    def unapply(nat: Succ): Option[Nat]
  }

  given Typeable[Nat, Zero] {
    def unapply(x: Nat): Option[Zero] = matchZero(x)
  }

  given Typeable[Nat, Succ] {
    def unapply(x: Nat): Option[Succ] = matchSucc(x)
  }

  protected def matchZero(x: Nat): Option[Zero]
  protected def matchSucc(x: Nat): Option[Succ]
}
```

it will be possible to write the following program

```scala
val peano: Peano = ...
import peano.{_, given}

def divOpt(m: Nat, n: Nat): Option[(Nat, Nat)] = {
  n match {
    case Zero => None
    case s @ Succ(_) => Some(safeDiv(m, s))
  }
}

val two = Succ(Succ(Zero))
val five = Succ(Succ(Succ(two)))
println(divOpt(five, two))
```

Note that without the `Typeable[Nat, Succ]` the pattern `Succ.unapply` would be unchecked.
