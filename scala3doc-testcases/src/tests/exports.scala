package tests
package exports 

class A:
  def aDefInt: Int = 1
  def aDef1: 1 = 1
  val aValInt: Int = 1
  val aVal1: 1 = 1
  var aVarInt: Int = 1
  var aVar1: 1 = 1

object X:
  def xDefInt: Int = 1
  def xDef1: 1 = 1
  val xValInt: Int = 1
  val xVal1: 1 = 1
  var xVarInt: Int = 1
  var xVar1: 1 = 1

class B:
  val a = new A
  export a._
  export X._
