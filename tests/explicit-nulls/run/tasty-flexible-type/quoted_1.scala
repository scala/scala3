import scala.quoted.*

object Macros {

  implicit inline def printTree[T](inline x: T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*

    val tree = x.asTerm
    val treeStr = Expr(tree.show(using Printer.TreeStructure))
    val treeTpeStr = Expr(tree.tpe.show(using Printer.TypeReprStructure))

    '{
      println(${treeStr})
      println(${treeTpeStr})
      println()
    }
  }

  inline def theTestBlock : Unit = ${ theTestBlockImpl }

  def theTestBlockImpl(using qctx : Quotes) : Expr[Unit] = {
    import quotes.reflect.*

    val ft1 = FlexibleType(TypeRepr.of[String])
    val ft1e = Expr(ft1.show(using Printer.TypeReprStructure))

    '{
      println(${ft1e})
    }
  }
}
