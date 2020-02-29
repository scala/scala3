object A:
  def f: String = ""

trait B:
  def f: String = "abc"

trait B2 extends B:
  override def f: String = "abc"

object D extends B:
  object b extends B
  export b._             // ok

object D1 extends B:
  object b extends B
  export b.f             // error

object D2 extends B:
  object b2 extends B2
  export b2.f            // error
