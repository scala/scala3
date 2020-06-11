object Foo:
  @scala.annotation.alpha("w") def \/\/ = "W"

object Bar:
  export Foo._

@main def Test =
  assert(Foo.getClass.getMethods.exists(_.getName == "w"))
  assert(Bar.getClass.getMethods.exists(_.getName == "w"))
