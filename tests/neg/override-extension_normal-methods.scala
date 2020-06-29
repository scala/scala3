trait A {
  def [T](t: T).m: String = "extension method"
}

trait AAA extends A {
  override def m[T](x: T): String  = "normal method" // error: does not override
}

trait B {
  def m[T](x: T): String  = "normal method"
}

trait BBB extends B {
  override def [T](t: T).m: String = "extension method" // error: does not override
}