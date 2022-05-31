object Base {
  trait Trait1
  trait Trait2
  type N[t] = t match {
    case String => Trait1
    case Int => Trait2
  }
}
import Base.*

object UpperBoundParametricVariant {
  trait Cov[+T]
  type M[t] = t match {
    case Cov[x] => N[x]
  }

  trait Root[A] {
    def thing: M[A]
  }

  trait Child[A <: Cov[Int]] extends Root[A]

  // we reduce `M[T]` to `Trait2`, even though we cannot be certain of that
  def foo[T <: Cov[Int]](c: Child[T]): Trait2 = c.thing

  class Asploder extends Child[Cov[String & Int]] {
    def thing = new Trait1 {} // error
  }

  def explode = foo(new Asploder) // ClassCastException
}

object InheritanceVariant {
  // allows binding a variable to the UB of a type member
  type Trick[a] = { type A <: a }
  type M[t] = t match { case Trick[a] => N[a] }

  trait Root {
    type B
    def thing: M[B]
  }

  trait Child extends Root { type B <: { type A <: Int } }

  def foo(c: Child): Trait2 = c.thing

  class Asploder extends Child {
    type B = { type A = String & Int }
    def thing = new Trait1 {} // error
  }
}

object ThisTypeVariant {
  type Trick[a] = { type A <: a }
  type M[t] = t match { case Trick[a] => N[a] }

  trait Root {
    def thing: M[this.type]
  }

  trait Child extends Root { type A <: Int }

  def foo(c: Child): Trait2 = c.thing

  class Asploder extends Child {
    type A = String & Int
    def thing = new Trait1 {} // error
  }
}

object ParametricVariant {
  type Trick[a] = { type A <: a }
  type M[t] = t match { case Trick[a] => N[a] }

  trait Root[B] {
    def thing: M[B]
  }

  trait Child[B <: { type A <: Int }] extends Root[B]

  def foo[T <: { type A <: Int }](c: Child[T]): Trait2 = c.thing

  class Asploder extends Child[{ type A = String & Int }] {
    def thing = new Trait1 {} // error
  }

  def explode = foo(new Asploder)
}

object UpperBoundVariant {
  trait Cov[+T]
  type M[t] = t match { case Cov[t] => N[t] }

  trait Root {
    type A
    def thing: M[A]
  }

  trait Child extends Root { type A <: Cov[Int] }

  def foo(c: Child): Trait2 = c.thing

  class Asploder extends Child {
    type A = Cov[String & Int]
    def thing = new Trait1 {} // error
  }

  def explode = foo(new Asploder)
}
