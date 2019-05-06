class Foo {
  import scala.quoted._
  import scala.quoted.matching._
  def f(sc: quoted.Expr[StringContext]) given tasty.Reflection: Unit = {

    val '{ StringContext(${parts}: _*) } = sc
    val ps0: Expr[Seq[String]] = parts

    val '{ StringContext(${ExprSeq(parts2)}: _*) } = sc
    val ps: Seq[Expr[String]] = parts2
  }
}