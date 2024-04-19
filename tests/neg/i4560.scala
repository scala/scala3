object Test1:
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
    type SuperBar
    type Foo <: Any { type Tie <: Test[Bar] & Test[o.Nested] & Test[o.OtherNested]; type U <: Int }

    val a: Foo = ???
    val b: a.U = ???
    val c: String = b // error: type mismatch

    val x: Bar = ???
    val y: x.U = ???
    val z: String = y // error: type mismatch


object Test2:
  class C0: // error: cyclic
    type T <: C1#T // error: cyclic

  class C1:
    type T <: C2#T // error: cyclic // error: cyclic

  class C2:
    type T <: C3#T // error: cyclic // error: cyclic

  class C3: // error: cyclic
    type T <: C0#T // error: cyclic // error: cyclic
