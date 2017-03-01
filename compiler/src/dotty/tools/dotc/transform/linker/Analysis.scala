package dotty.tools.dotc.transform.linker

import dotty.tools.dotc.{ast, core}
import core._
import Contexts._
import dotty.tools.dotc.ast.Trees._
import StdNames._
import NameOps._
import dotty.tools.dotc.ast.tpd
import Symbols._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Types.{NoPrefix, TermRef, ThisType}
import dotty.tools.dotc.transform.{Erasure, TreeTransforms}
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}
import dotty.tools.dotc.transform.SymUtils._
import Decorators._

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
      case _ =>
        false
      // analisys
    }
  }
}
