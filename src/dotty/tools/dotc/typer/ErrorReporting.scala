package dotty.tools
package dotc
package typer

import ast._
import core._
import Trees._
import Types._, Contexts._, Decorators._, Denotations._
import Applications._
import util.Positions._

object ErrorReporting {

  import tpd._

  def errorTree(tree: Tree, msg: => String)(implicit ctx: Context): tpd.Tree =
    tree withType errorType(msg, tree.pos)

  def errorType(msg: => String, pos: Position)(implicit ctx: Context): ErrorType = {
    ctx.error(msg, pos)
    ErrorType
  }

  class Errors(implicit ctx: Context) {

    def expectedTypeStr(tp: Type): String = tp match {
      case tp: FunProtoType =>
        val result = tp.resultType match {
          case tp: WildcardType => ""
          case tp => s"and expected result type $tp"
        }
        s"arguments (${tp.typedArgs map (_.tpe.show) mkString ", "})$result"
      case _ =>
        s"expected type ${tp.show}"
    }

    def anonymousTypeMemberStr(tpe: Type) = {
      val kind = tpe match {
          case _: TypeBounds => "type with bounds"
          case _: PolyType | _: MethodType => "method"
          case _ => "value of type"
        }
        s"$kind $tpe"
    }

    def overloadedAltsStr(alts: List[SingleDenotation]) =
      s"overloaded alternatives of ${denotStr(alts.head)} with types\n" +
      s" ${alts map (_.info) mkString "\n "}"

    def denotStr(denot: Denotation): String =
      if (denot.isOverloaded) overloadedAltsStr(denot.alternatives)
      else if (denot.symbol.exists) denot.symbol.showLocated
      else anonymousTypeMemberStr(denot.info)

    def refStr(tp: Type): String = tp match {
      case tp: NamedType => denotStr(tp.denot)
      case _ => anonymousTypeMemberStr(tp)
    }

    def exprStr(tree: Tree): String = refStr(tree.tpe)

    def patternConstrStr(tree: Tree): String = ???

    def typeMismatch(tree: Tree, pt: Type): Tree =
      errorTree(tree,
        s"""type mismatch:
           | found   : ${tree.tpe.show}
           | required: ${pt.show}""".stripMargin)

  }

  def err(implicit ctx: Context): Errors = new Errors
}