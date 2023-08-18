//> using options -source 3.0-migration -Xfatal-warnings

class A {
  val x: Int = 1
}
class B extends A {
  override lazy val x: Int = 2 // error
}
