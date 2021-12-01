case class Case(b: Base)

class Base {
  val n = 10
  val f = Case(this)

  val m = 10
}
