object Test1:
  trait SimpleTrait:
    type A <: { type T = B }
    type B <: A

  trait SimpleSubTrait extends SimpleTrait:
    val v: A


object Test2:
  trait Trait:
    type A <: { type T = B }
    type B <: { type S = A }

  trait SubTrait extends Trait:
    val v: A

  class XA:
    type T = XB

  class XB:
    type S = XA

  class Foo extends Trait:
    type A = XA
    type B = XB


object Test3:
  trait Test[T]

  trait Abstract[T]:
    type Foo <: Any { type Tie <: Test[Bar] & Test[o.Nested] }
    type Bar <: Any { type Tie <: Test[Foo] }

    object o:
      type Nested <: Foo { type Tie <: Test[Bar] & Test[Nested] & Test[OtherNested] }
      type OtherNested <: Any { type Tie <: Test[Nested] }

  trait AbstractIntermediate[T] extends Abstract[T]:
    type Bar <: Any { type Tie <: Test[Foo]; type U <: T }

  trait ConcreteIntermediate extends AbstractIntermediate[Int]:
    type Foo <: Any { type Tie <: Test[Bar] & Test[o.Nested] & Test[o.OtherNested]; type U <: Int }

  object concrete extends ConcreteIntermediate:
    type SuperFoo
    type Foo <: SuperFoo { type Tie <: Test[Bar] & Test[o.Nested] & Test[o.OtherNested]; type U <: Int }

    val a: Foo = ???
    val b: a.U = ???
    val c: Int = b

    val x: Bar = ???
    val y: x.U = ???
    val z: Int = y


object Test4:
  class C0:
    type T <: { type X <: C1#T }

  class C1:
    type T <: { type X <: C2#T }

  class C2:
    type T <: { type X <: C3#T }

  class C3:
    type T <: { type X <: C0#T }
