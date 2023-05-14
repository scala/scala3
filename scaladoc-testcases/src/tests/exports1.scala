package tests
package exports1

class A: //unexpected
  def aDefInt: Int
   = 1
  def aDef1: 1
   = 1
  val aValInt: Int
   = 1
  val aVal1: 1
   = 1
  var aVarInt: Int
   = 1
  var aVar1: 1
   = 1
  type HKT[T[_], X] //expected: final type HKT = [T[_], X] =>> HKT[T, X]
   = T[X]
  type SomeRandomType = (List[_] | Seq[_]) & String //expected: final type SomeRandomType = SomeRandomType
  def x[T[_], X](x: X): HKT[T, X]
   = ???
  def fn[T, U]: T => U
   = ???
  object Object //expected: val Obj: Object.type
  val x: HKT[List, Int]
   = ???
  class Class(val a: Int, val b: Int) extends Serializable //expected: final type Class = Class
  enum Enum: //expected: final type Enum = Enum
    case A
    case B(i: Int)
    case C[T]() extends Enum

object X: //unexpected
  def xDefInt: Int
   = 1
  def xDef1: 1
   = 1
  val xValInt: Int
   = 1
  val xVal1: 1
   = 1
  var xVarInt: Int
   = 1
  var xVar1: 1
   = 1