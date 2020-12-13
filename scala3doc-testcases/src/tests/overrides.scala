package tests
package overrides

class A:
  def defInt: Int = 1

class B extends A:
  override def defInt: Int = 2

class C extends B:
  override def defInt: Int = 3
