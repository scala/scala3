import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Universe
import scala.tasty.util.TastyPrinter

object Asserts {

  implicit class Ops[T](left: T) {
    def ===(right: T): Boolean = left == right
    def !==(right: T): Boolean = left != right
  }

  object Ops

  inline def macroAssert(cond: Boolean): Unit =
    ~impl('(cond))(Universe.compilationUniverse) // FIXME infer Universe.compilationUniverse within top level ~

  def impl(cond: Expr[Boolean])(implicit u: Universe): Expr[Unit] = {
    import u._
    import u.tasty._

    val tree = cond.toTasty

    def isOps(tpe: MaybeType): Boolean = tpe match {
      case SymRef(DefDef("Ops", _, _, _, _), _) => true // TODO check that the parent is Asserts
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
      case Apply(Select(OpsTree(left), op, _), right :: Nil) =>
        // FIXME splice the threes directly
        val lExpr = TastyPrinter.stringOfTree(tasty)(left).toExpr
        val rExpr = TastyPrinter.stringOfTree(tasty)(right).toExpr
        op match {
          case "===" => '(assertEquals(~lExpr, ~rExpr))
          case "!==" => '(assertNotEquals(~lExpr, ~rExpr))
        }
      case _ =>
        '(assertTrue(~cond))
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
