package tests
package deprecated 

@Deprecated
class A:
  def defInt: Int = 1
  def def1: 1 = 1
  val valInt: Int = 1
  val val1: 1 = 1
  var varInt: Int = 1
  var var1: 1 = 1
  class InnerA:
    val innerVal: Int = 1

class B:
  @Deprecated
  def x: Int = 1
  val y: Int = 1
