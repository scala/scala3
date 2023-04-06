// from Scala.js
object Names {   // error
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
