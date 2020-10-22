import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
    staging.run(genCode)
  }
}

inline def isTrue1: Boolean = true

object Foo:
  inline def isTrue2: Boolean = true

inline def isTrue3(): Boolean = true

inline def oneOf: String =
  inline if isTrue1 then "foo" else "bar"
  inline if Foo.isTrue2 then "foo" else "bar"
  inline if isTrue3() then "foo" else "bar"

def genCode(using Quotes) = '{ oneOf }
