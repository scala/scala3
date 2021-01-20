object Foo with
  inline def get = 0

object Bar with
  export Foo._

val v = Bar.get
