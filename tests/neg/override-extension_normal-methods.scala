trait A {
  extension [T](t: T) def m: String = "extension method"
}

trait AAA extends A {
  override def m[T](x: T): String  = "normal method" // error: does not override
}

trait B {
  def m[T](x: T): String  = "normal method"
}

trait BBB extends B {
  extension [T](t: T) override def m: String = "extension method" // error: does not override
}