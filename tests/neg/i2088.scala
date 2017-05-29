class Base {
  type A = { // error: illegal cyclic reference: alias Object{m: Foo.B} of type B refers back to the type itself
    val m: Foo.A
  }

  protected type B = { // error: illegal cyclic reference: alias Object{m: Foo.B} of type B refers back to the type itself
    val m: Foo.B
  }

  private type C = {
    val m: Foo.C // error: type `C` is not a member of Foo.type
  }

  type D = { // error: illegal cyclic reference: alias Object{m: Foo.E} of type D refers back to the type itself
    val m: Foo.E
  }

  type E = {
    val m: Foo.D
  }
}

object Foo extends Base
