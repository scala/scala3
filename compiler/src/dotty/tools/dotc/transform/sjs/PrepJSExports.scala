package dotty.tools.dotc
package transform
package sjs

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import scala.collection.mutable
import core._
import dotty.tools.dotc.typer.Checking
import dotty.tools.dotc.typer.Inliner
import dotty.tools.dotc.typer.VarianceChecker
import Types._
import Contexts._
import Names._
import Flags._
import DenotTransformers._
import Phases._
import SymDenotations._
import StdNames._
import Annotations._
import Trees._
import Scopes._
import Decorators._
import Symbols._
import SymUtils._
import NameOps._
import reporting._
import util.SrcPos

import dotty.tools.backend.sjs.JSDefinitions.jsdefn

object PrepJSExports {
  import tpd._

  def registerClassOrModuleExports(sym: Symbol)(using Context): Unit = {
    // TODO
  }

  /** Generate the exporter for the given DefDef
   *  or ValDef (abstract val in class, val in trait or lazy val;
   *  these don't get DefDefs until the fields phase)
   *
   *  If this DefDef is a constructor, it is registered to be exported by
   *  GenJSCode instead and no trees are returned.
   */
  def genExportMember(baseSym: Symbol)(using Context): List[Tree] = {
    // TODO
    Nil
  }
}
