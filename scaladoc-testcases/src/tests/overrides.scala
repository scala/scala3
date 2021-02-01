package tests
package overrides

trait  A:
  def defInt: Int = 1

trait B extends A:
  override def defInt: Int = 2

trait C extends B

class D extends C:
  override def defInt: Int = 4
