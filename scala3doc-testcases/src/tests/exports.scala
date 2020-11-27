package tests
package exports 

class A:
  def defInt: Int = 1
  def def1: 1 = 1
  val valInt: Int = 1
  val val1: 1 = 1
  var varInt: Int = 1
  var var1: 1 = 1

class B:
  val a = new A
  export a._
