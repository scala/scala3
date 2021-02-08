import scala.quoted.*

object Asserts {

  implicit class Ops[T](left: T) {
    def ===(right: T): Boolean = left == right
    def !==(right: T): Boolean = left != right
  }

  object Ops

  inline def macroAssert(inline cond: Boolean): Unit =
    ${impl('cond)}

  def impl(cond: Expr[Boolean])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*

    val tree = cond.asTerm

    def isOps(tpe: TypeRepr): Boolean = tpe match {
      case tpe: TermRef => tpe.termSymbol.isDefDef && tpe.name == "Ops"// TODO check that the parent is Asserts
      case _ => false
    }

    object OpsTree {
      def unapply(arg: Term): Option[Term] = arg match {
        case Apply(TypeApply(term, _), left :: Nil) if isOps(term.tpe) =>
          Some(left)
        case _ => None
      }
    }

    tree match {
      case Inlined(_, Nil, Apply(Select(OpsTree(left), op), right :: Nil)) =>
        '{assertTrue(${left.asExprOf[Boolean]})} // Buggy code. To generate the errors
      case _ =>
        '{assertTrue($cond)}
    }

  }

  def assertEquals[T](left: T, right: T): Unit = {
    if (left != right) {
      println(
        s"""Error left did not equal right:
           |  left  = $left
           |  right = $right""".stripMargin)
    }

  }

  def assertNotEquals[T](left: T, right: T): Unit = {
    if (left == right) {
      println(
        s"""Error left was equal to right:
           |  left  = $left
           |  right = $right""".stripMargin)
    }

  }

  def assertTrue(cond: Boolean): Unit = {
    if (!cond)
      println("Condition was false")
  }

}
