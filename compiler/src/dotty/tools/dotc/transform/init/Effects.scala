package dotty.tools.dotc
package transform
package init

import core._
import Contexts.Context
import StdNames._
import Names._
import ast._
import Trees._
import Symbols._
import Types._
import Decorators._
import util.Positions._
import config.Printers.init.{ println => debug }
import collection.mutable

//=======================================
//           Res
//=======================================
import Effect._

case class Res(var effects: Effects = Vector.empty, var value: Value = FullValue) {
  def +=(eff: Effect): Unit = effects = effects :+ eff

  def ++=(effs: Effects): Unit =
    effects ++= effs

  def +(eff: Effect): this.type = {
    effects = effects :+ eff
    this
  }

  def ++(effs: Effects): this.type = {
    effects ++= effs
    this
  }

  def hasErrors  = effects.size > 0

  def join(res2: Res): Res =
    Res(
      effects = res2.effects ++ this.effects,
      value   = res2.value.join(value)
    )

  def show(setting: ShowSetting)(implicit ctx: Context): String =
    s"""~Res(
        ~| effects = ${if (effects.isEmpty) "()" else effects.mkString("\n|    - ", "\n|    - ", "")}
        ~| value   = ${value.show(setting)}
        ~)"""
    .stripMargin('~')
}


//=======================================
//             Effects
//=======================================

sealed trait Effect {
  def report(implicit ctx: Context): Unit = this match {
    case Uninit(sym, pos)         =>
      ctx.warning(s"Reference to uninitialized value `${sym.name}`", pos)
    case OverrideRisk(sym, pos)     =>
      ctx.warning(s"Reference to $sym which could be overriden. Consider make the method final or annotate it with `@partial` or `@filled` for safe overriding", pos)
    case Call(sym, effects, pos)  =>
      ctx.warning(s"The call to `${sym.name}` causes initialization problem", pos)
      effects.foreach(_.report)
    case Force(sym, effects, pos) =>
      ctx.warning(s"Forcing lazy val `${sym.name}` causes initialization problem", pos)
      effects.foreach(_.report)
    case Instantiate(cls, effs, pos)  =>
      ctx.warning(s"Create instance results in initialization errors", pos)
      effs.foreach(_.report)
    case UseAbstractDef(sym, pos)  =>
      ctx.warning(s"`@scala.annotation.init` is recommended for abstract $sym for safe initialization", sym.pos)
      ctx.warning(s"Reference to abstract $sym which should be annotated with `@scala.annotation.init`", pos)
    case Generic(msg, pos) =>
      ctx.warning(msg, pos)
  }
}

case class Uninit(sym: Symbol, pos: Position) extends Effect                         // usage of uninitialized values
case class OverrideRisk(sym: Symbol, pos: Position) extends Effect                   // calling methods that are not override-free
case class Call(sym: Symbol, effects: Seq[Effect], pos: Position) extends Effect     // calling method results in error
case class Force(sym: Symbol, effects: Seq[Effect], pos: Position) extends Effect    // force lazy val results in error
case class Instantiate(cls: Symbol, effs: Seq[Effect], pos: Position) extends Effect // create new instance of in-scope inner class results in error
case class UseAbstractDef(sym: Symbol, pos: Position) extends Effect                 // use abstract def during initialization, see override5.scala
case class Generic(msg: String, pos: Position) extends Effect                        // generic problem

object Effect {
  type Effects = Vector[Effect]
}