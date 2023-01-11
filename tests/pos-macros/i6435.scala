class Foo {
  import scala.quoted.*

  def f(sc: quoted.Expr[StringContext])(using Quotes): Unit = {

    val '{ StringContext(${parts}*) } = sc: @unchecked
    val ps0: Expr[Seq[String]] = parts

    val '{ StringContext(${Varargs(parts2)}*) } = sc: @unchecked
    val ps: Seq[Expr[String]] = parts2
  }
}
