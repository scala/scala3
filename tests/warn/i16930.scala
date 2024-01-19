//> using options  -Wunused:imports

trait Outer:
  trait Used
  trait Unused

object Test {
  val outer: Outer = ???
  import outer.{Used, Unused} // warn
  def foo(x: Any): Used = x.asInstanceOf[Used]
}

trait Outer1:
  trait UnusedToo1
  trait Unused1
  def unusedToo1: UnusedToo1

object Test1 {
  val outer1: Outer1 = ???
  import outer1.{Unused1, UnusedToo1} // warn // warn
  def foo() = outer1.unusedToo1 // in this case UnusedToo1 is not used explicitly, only inferred
}