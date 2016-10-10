package test

import dotty.tools.dotc.util._
import Positions._

object positiontest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val p = Position(0, 1, 0)                       //> p  : dotty.tools.dotc.util.Positions.Position = [0..1]
  val p2 = Position(0, 2)                         //> p2  : dotty.tools.dotc.util.Positions.Position = [0..2]
  val p3 = Position(1, 0)                         //> p3  : dotty.tools.dotc.util.Positions.Position = [no position]
  p3.isSynthetic                                  //> res0: Boolean = false
  NoPosition.isSynthetic                          //> res1: Boolean = false
}