//> using options -Werror -deprecation -feature

import scala.annotation.switch

class Test {
  import Test.*

  def test1(x: Int): Int = (x: @switch) match {
    case 1 => 1
    case 2 | 3 | 4 => 2
    case 65 => 3
    case 72 => 4
  }

  def test2(c: Char): Boolean = (c: @switch) match {
    case LF | CR | FF | SU => true
    case _ => false
  }

  // #1313
  def test3(x: Int, y: Int): Int = (x: @switch) match {
    case 6 if y > 5 => 1
    case 6 => 2
    case 12 => 3
    case 14 => 4
    case _ => 5
  }

  def test4(x: Byte): Boolean = (x: @switch) match {
    case 1 | 2 | 3 => true
    case _ => false
  }

  def test5(x: Short): Boolean = (x: @switch) match {
    case 1 | 2 | 3 => true
    case _ => false
  }

  def test6(x: IntAnyVal) = (x: @switch) match {
    case IntAnyVal(1) => 0
    case IntAnyVal(10) => 1
    case IntAnyVal(100) => 2
    case IntAnyVal(1000) => 3
    case IntAnyVal(10000) => 4
    case _ => -1
  }
}

case class IntAnyVal(x: Int) extends AnyVal

object Test {
  final val LF = '\u000A'
  final val CR = '\u000D'
  final val FF = '\u000C'
  final val SU = '\u001A'
}
