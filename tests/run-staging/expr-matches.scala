import scala.quoted._
import scala.quoted.staging._


object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    assert('{1} matches '{1})
    assert('{println("foo")} matches '{println("foo")})
    assert('{println("foo")} matches '{println(${Expr("foo")})})
    assert('{println(Some("foo"))} matches '{println(${ val a = '{Some("foo")}; a})})
  }
}
