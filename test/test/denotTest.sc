package test

import dotty.compiler.internal._
import core._
import Decorators._
import Types._, Symbols._

object denotTest extends DottyTest {
  println("Welcome to the Scala worksheet")
  
  val str = defn.StringClass.typeRef
  val d= str.member("getBytes".toTermName)
  d.alternatives
  d.alternatives.map(_.info)
  val sm = defn.StringClass.companionModule
  val d2 = sm.info.member("valueOf".toTermName)
	d2.alternatives.map(_.info)
}