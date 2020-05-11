class Foo {
  import scala.quoted._

  def f(using s: Scope)(sc: s.Expr[StringContext]): Unit = {

    val '{ StringContext(${parts}: _*) } = sc
    val ps0: s.Expr[Seq[String]] = parts

    val '{ StringContext(${Varargs(parts2)}: _*) } = sc
    val ps: Seq[s.Expr[String]] = parts2
  }
}