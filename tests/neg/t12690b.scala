
// scalac: -Werror -Wunused:imports

class X
class Y extends X
object A { implicit val x: X = new X }
object B { implicit val y: Y = new Y }
class C {
  import A._ // error: unused
  import B._
  def t = implicitly[X]
}

object Test extends App {
  println(new C().t)
}
