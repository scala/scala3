package x
import dotty.compiler.internal._
import core._
import Contexts._
import Symbols._
import Decorators._

object xplore {
  println("Welcome to the Scala worksheet")
  val c = Main.newCompiler
  val base = new ContextBase
  implicit val ctx = c.rootContext(base.initialCtx)
	val strClass = defn.StringClass
	strClass.baseClasses
  strClass.typeRef <:< defn.AnyType
  val predef = defn.DottyPredefModule
  val strd = predef.info.member("String".toTypeName)
  strd.info
  val strType = strd.symbol.typeRef
  strType <:< defn.AnyType
  val prdef = defn.DottyPredefModule
  predef.isCompleted
  predef.info.parents
  predef.info
	predef.info.typeSymbol
	val scala = defn.ScalaPackageClass
	scala.info.decl("Predef$".toTypeName)
	predef.info.decls
}