package dotty.tools
package repl

import scala.util.control.NonFatal

import dotc.core.Types._
import dotc.core.Contexts.Context
import dotc.core.Denotations.Denotation
import dotc.core.Flags
import dotc.core.Symbols.Symbol

object Rendering {
  /** Load the value of the symbol using reflection */
  private[this] def valueOf(sym: Symbol, classLoader: ClassLoader)(implicit ctx: Context): String = {
    val objectName = "$none$." + sym.owner.name.show
    val resObj: Class[_] = Class.forName(objectName, true, classLoader)
    val objInstance = resObj.newInstance()
    objInstance
      .getClass()
      .getDeclaredMethods.find(_.getName == sym.name.show).get
      .invoke(objInstance).toString
  }

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

  def renderVal(d: Denotation, classLoader: ClassLoader)(implicit ctx: Context): String = {
    val prefix = if (d.symbol.is(Flags.Mutable)) "var" else "val"
    val tpe = d.info match {
      case ConstantType(c) => c.value.toString
      case tpe => tpe.show
    }
    val res = valueOf(d.symbol, classLoader)
    s"$prefix ${d.symbol.name.show}: $tpe = $res"
  }
}
