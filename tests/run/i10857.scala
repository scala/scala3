object Module:

  enum Foo:
    case Value
    case Parameterised(i: Int)

  export Foo._

@main def Test =
  import Module.given
  println(Module.Parameterised.apply(23)) // synthetic companion object is exported
  println(Module.Value)
