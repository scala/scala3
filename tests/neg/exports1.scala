object A with
  def f: String = ""

trait B with
  def f: String = "abc"

trait B2 extends B with
  override def f: String = "abc"

object D extends B with
  object b extends B
  export b._             // ok

object D1 extends B with
  object b extends B
  export b.f             // error

object D2 extends B with
  object b2 extends B2
  export b2.f            // error
