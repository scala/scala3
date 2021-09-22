private object Foo:
  inline def d(arg : Int): Unit = {}
  transparent inline def haha() : Unit = {}

export Foo.*

@main def main : Unit = haha()
