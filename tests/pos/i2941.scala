trait FooBase {
  type Bar >: Null <: BarBase { type This <: FooBase.this.Bar }
  type This >: this.type <: FooBase { type This <: FooBase.this.This }

  def derived(bar: Bar): This = ???
}

trait BarBase {
  type This >: Null <: BarBase { type This <: BarBase.this.This }
}

object Test {
  def bad(foo: FooBase): FooBase = foo match {
    case foo: FooBase =>
      foo.derived(???)  // Triggers infinite loop in TypeAssigner.avoid()
  }
}
