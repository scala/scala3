package dotty.tools.dotc
package transform.linker

import ast.Trees._
import ast.tpd
import core.Contexts._
import core.NameOps._
import core.StdNames._
import core.Symbols._
import dotty.tools.dotc.core.Flags

object Analysis {
  import tpd._
  private val constructorWhiteList = Set(
    "scala.Tuple2",
    "scala.Tuple3",
    "scala.Tuple4",
    "scala.Tuple5",
    "scala.Tuple6",
    "scala.Tuple7",
    "scala.Tuple8",
    "scala.Tuple9",
    "scala.Tuple10",
    "scala.Tuple11",
    "scala.Tuple12",
    "scala.Tuple13",
    "scala.Tuple14",
    "scala.Tuple15",
    "scala.Tuple16",
    "scala.Tuple17",
    "scala.Tuple18",
    "scala.Tuple19",
    "scala.Tuple20",
    "scala.Tuple21",
    "scala.Tuple22",
    "scala.Some"
  )

  private val moduleWhiteList = constructorWhiteList.map(x => x + "$")

  private val methodsWhiteList = List(
    "java.lang.Math.min",
    "java.lang.Math.max",
    "java.lang.Object.eq",
    "java.lang.Object.ne",
    "scala.Boolean.$amp$amp",
    "scala.runtime.BoxesRunTime.unboxToBoolean",
    "scala.runtime.BoxesRunTime.unboxToLong",
    "scala.runtime.BoxesRunTime.unboxToInt",
    "scala.runtime.BoxesRunTime.unboxToShort",
    "scala.runtime.BoxesRunTime.unboxToDouble",
    "scala.runtime.BoxesRunTime.unboxToChar",
    "scala.runtime.BoxesRunTime.unboxToFloat"
  )

  def effectsDontEscape(t: Tree)(implicit ctx: Context) = {
    t match {
      case Apply(fun, args) if fun.symbol.isConstructor && constructorWhiteList.contains(fun.symbol.owner.fullName.toString) =>
        true
      case Apply(fun, args) if methodsWhiteList.contains(fun.symbol.fullName.toString) =>
        true
      case Ident(_) if t.symbol.is(Flags.Module) && (t.symbol.is(Flags.Synthetic) || moduleWhiteList.contains(t.symbol.fullName.toString)) =>
        true
      case _ =>
        false
      // analisys
    }
  }
}
