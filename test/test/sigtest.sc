package test

import dotty.compiler.internal._
import core._
import Decorators._
import Types._, Symbols._

object sigtest extends DottyTest {
  println("Welcome to the Scala worksheet")
  val int = ctx.requiredClass("scala.Int")
  int.signature
  val intmeth = methType("x")(int.symbolicRef)() // fails to compile
  intmeth.signature
  val arr = defn.ArrayType.appliedTo(int.symbolicRef) // fails to compile
  val arraymeth = methType("x")(arr)()
  arraymeth.signature
  val curriedmeth = methType("x", "y")(defn.IntType, defn.BooleanType)(methType("z")(defn.ArrayType.appliedTo(defn.IntType))())
  curriedmeth.signature
}