trait A {
  def [T](t: T).m: String = "extrnsion method"
}

trait AAA extends A {
  def m[T](x: T): String  = "normal method" // error: normal method, cannot override an extension method. Also needs `override' modifier (but this error should be obfuscated).
}

trait B {
  def m[T](x: T): String  = "normal method"
}

trait BBB extends B {
  def [T](t: T).m: String = "extrnsion method" // error: extension method, cannot override an normal method. Also needs `override' modifier (but this error should be obfuscated).
}