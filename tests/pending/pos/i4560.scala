class Ref

class C { type T }

trait Trait {
  type A <: C { type T = B }
  type B <: A
}

trait SubTrait extends Trait {
  val v: A
}