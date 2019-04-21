object Macros {
  import scala.quoted._
  import scala.quoted.autolift._
  import scala.tasty._

  inline def go[T](t: => T) = ${ impl('t) }
  def impl[T](expr: Expr[T])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._

    val tree = expr.unseal

    val methods =
      tree.tpe.classSymbol.get.classMethods.map { m =>
        val name = m.show
        val returnType = m.tree.returnTpt.tpe.show
        s"$name : $returnType"
      }.sorted

    methods.foldLeft('{}) { (res, m) => '{ $res; println(${m}) } }
  }
}
