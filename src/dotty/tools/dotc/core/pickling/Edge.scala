package dotty.tools.dotc
package core.pickling

import util.Positions._
import ast.tpd._
import core.Contexts._

abstract class Edge {
  def offset(pos: Position): Int
  def seq(op1: () => Unit, op2: () => Unit): Unit
}

object Edge {
  object left extends Edge {
    def offset(pos: Position): Int = pos.start
    def seq(op1: () => Unit, op2: () => Unit) = { op1(); op2() }
  }
  object right extends Edge {
    def offset(pos: Position): Int = pos.end
    def seq(op1: () => Unit, op2: () => Unit) = { op2(); op1() }
  }
}