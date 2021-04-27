package tests
package deprecated

class A:
  def defInt: Int = 1
  @deprecated(message = "1")
  def def1: 1 = 1
  @deprecated("reason")
  val valInt: Int = 1
  val val1: 1 = 1
  var varInt: Int = 1
  var var1: 1 = 1
  class InnerA:
    val innerVal: Int = 1

class B extends A:
  @deprecated(since = "1", message = "some reason")
  def x: Int = 1
  val y: Int = 1


@java.lang.Deprecated
class JavaDeprecated
