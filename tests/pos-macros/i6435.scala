class Foo {
  import scala.quoted.*

  def f(sc: quoted.Expr[StringContext])(using Quotes): Unit = {

    val '{ StringContext(${parts}*) } = sc
    val ps0: Expr[Seq[String]] = parts

    val '{ StringContext(${Varargs(parts2)}*) } = sc
    val ps: Seq[Expr[String]] = parts2
  }
}