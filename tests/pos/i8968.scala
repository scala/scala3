object Foo:
  inline def get = 0

object Bar:
  export Foo._

val v = Bar.get
