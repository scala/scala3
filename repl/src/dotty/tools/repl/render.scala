package dotty.tools
package repl

import dotc.core.Types._
import dotc.core.Contexts.Context
import dotc.core.Denotations.Denotation
import dotc.core.Flags

object Rendering {

  def renderMethod(d: Denotation)(implicit ctx: Context): String = {
    def params(tpe: Type): String = tpe match {
      case ConstantType(c) => ": " + c.value
      case tpe: ParamRef => ": " + tpe.paramName + ".type"
      case tpe: TypeRef => ": " + tpe.show
      case mt: MethodType =>
        val ps = mt.paramNames.zip(mt.paramInfos).map { (name, tpe) =>
          val tp = tpe match {
            case ConstantType(c) => c.value.toString
            case tpe => tpe.show
          }
          s"${name.show}: $tp"
        }
        val plist =
          if (mt.resultType.isInstanceOf[ExprType]) ""
          else "(" + ps.mkString(", ") + ")"

        plist + params(mt.resultType)
    }

    s"def ${d.symbol.name.show}${params(d.info)}"
  }

  def renderVal(d: Denotation)(implicit ctx: Context): String = {
    val prefix = if (d.symbol.is(Flags.Mutable)) "var" else "val"
    val tpe = d.info match {
      case ConstantType(c) => c.value.toString
      case tpe => tpe.show
    }
    s"$prefix ${d.symbol.name.show}: $tpe"
  }

}
