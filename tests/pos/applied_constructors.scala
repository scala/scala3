import scala.language.experimental.modularity

class C(tracked val x: Int)
class D(tracked val c: C)

object Test extends App {
  val c: C(42) = C(42)
  // val d: D(C(42)) = D(C(42))
}
