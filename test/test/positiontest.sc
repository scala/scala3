package test

import dotty.compiler.internal.util._
import Positions._

object positiontest {
  println("Welcome to the Scala worksheet")
  
  val p = Position(0, 1, 0)
  val p2 = Position(0, 2)
  val p3 = Position(1, 0)
  p3.isSynthetic
  NoPosition.isSynthetic
}