package x
import dotty.tools.dotc._
import core._
import Contexts._
import Symbols._
import Decorators._

object xplore {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val c = Main.newCompiler                        //> c  : dotty.tools.dotc.Compiler = dotty.tools.dotc.Compiler@5705b99f
  val base = new ContextBase                      //> base  : dotty.tools.dotc.core.Contexts.ContextBase = dotty.tools.dotc.core.C
                                                  //| ontexts$ContextBase@30419d05
  implicit val ctx = c.rootContext(base.initialCtx)
                                                  //> ctx  : dotty.tools.dotc.core.Contexts.Context = dotty.tools.dotc.core.Contex
                                                  //| ts$InitialContext@1a9d267d
	val strClass = defn.StringClass           //> strClass  : dotty.tools.dotc.core.Symbols.ClassSymbol = class String#205
	strClass.baseClasses                      //> res0: List[dotty.tools.dotc.core.Symbols.ClassSymbol] = List(class String#20
                                                  //| 5, class CharSequence#523, class Comparable#94, class Serializable#4192, cla
                                                  //| ss Object#121, class Any#2416)
  strClass.typeConstructor <:< defn.AnyType       //> res1: Boolean = true
  val predef = defn.PredefModule                  //> predef  : dotty.tools.dotc.core.Symbols.TermSymbol = module Predef#1627
  val strd = predef.info.member("String".toTypeName)
                                                  //> strd  : dotty.tools.dotc.core.Denotations.Denotation = type String
  strd.info                                       //> res2: dotty.tools.dotc.core.Types.Type = TypeAlias(TypeRef(ThisType(module c
                                                  //| lass lang#49),String))
  val strType = strd.symbol.typeConstructor       //> strType  : dotty.tools.dotc.core.Types.TypeRef = TypeRef(ThisType(module cla
                                                  //| ss Predef$#1628),String)
  strType <:< defn.AnyType                        //> res3: Boolean = true
}