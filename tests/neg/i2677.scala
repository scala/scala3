trait A { def x = "foo" }
trait B { def x = 42 }
object Test {
  val AB = new A with B { override def x = super.x } // error: wrong override
  AB.x
}