import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.trees._
import scala.tasty.types._
import scala.tasty.names._
import scala.tasty.Context
import scala.tasty.util.TastyPrinter

object Asserts {

  implicit class Ops[T](left: T) {
    def ===(right: T): Boolean = left == right
    def !==(right: T): Boolean = left != right
  }

  object Ops

  inline def macroAssert(cond: Boolean): Unit =
    ~impl('(cond))(Context.compilationContext) // FIXME infer Context.compilationContext within top level ~

  def impl(cond: Expr[Boolean])(implicit ctx: Context): Expr[Unit] = {
    val tree = cond.toTasty

    def isOps(tpe: MaybeType): Boolean = tpe match {
      case SymRef(DefDef(Simple("Ops"), _, _, _, _), _) => true // TODO check that the parent is Asserts
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
      case Apply(Select(OpsTree(left), Simple(op)), right :: Nil) =>
        // FIXME splice the threes directly
        val lExpr = TastyPrinter.stringOf(left).toExpr
        val rExpr = TastyPrinter.stringOf(right).toExpr
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
