// from Scala.js
object Names {
  private final val ConstructorSimpleEncodedName: String =
    "<init>"

  final class SimpleMethodName(encoded: String)

  object SimpleMethodName {
    def apply(name: String): SimpleMethodName =
      val res = name == ConstructorSimpleEncodedName
      new SimpleMethodName(name)
  }

  val ConstructorSimpleName: SimpleMethodName =
    SimpleMethodName(ConstructorSimpleEncodedName)
}

object A {              // error
  val n: Int = B.m
}

object B {
  val m: Int = A.n
}
