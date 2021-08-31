class broken {
  sealed trait Foo
  case object A extends Foo
  case object B extends Foo
  case object C extends Foo
  case object D extends Foo

  inline def foo(inline f: Foo) = inline f match {
    case _: A.type => "the letter a"
    case _: B.type => "the letter b"
    case _: C.type => "the letter c"
    case _: D.type => "the letter d"
  }

  inline def thingy(
    depthClampEnable: Boolean = false,
    rasterizerDiscardEnable: Boolean = false,
    polygonMode: Int = 0,
    cullMode: Int = 0,
    frontFace: Int = 0,
    depthBiasEnable: Boolean = false,
    depthBiasConstantFactor: Float = 0,
    depthBiasClamp: Float = 0,
    depthBiasSlopeFactor: Float = 0,
    lineWidth: Float = 0,
    inline f: Foo = A,
  ) = {
    foo(f)
  }

  thingy(polygonMode = Constants.A, cullMode = Constants.B, frontFace = Constants.C, lineWidth = 1.0f)
}
