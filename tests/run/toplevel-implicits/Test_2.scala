object Test extends App {
  import implicits.*

  val c = new C
  val c2 = c.pair(c)
  println(c2)

  val d = new D
  val d2 = d.pair(d)
  println(d2)

  val ci: C = "abc"
  implicitly[TC[List[Int]]]
}