trait A:
  def f: Int

class B extends A:
  @deprecatedOverriding def f = 1

class C extends B:
  override def f = 2  // error

trait D extends A:
  override def f = 3

object E extends B, D  // error

