import scala.language.experimental.modularity
import scala.language.future

class C(tracked val x: Int)
class D(tracked val c: C)
class E(tracked val c: D)
class F[A](tracked val a: Int)
class G[A](tracked val a: A)

object Test extends App {
  val c: C(42) = C(42)
  val d: D(C(42)) = D(C(42))
  val e: E(D(C(42))) = E(D(C(42)))
  // val f: F[Int](42) = F[Int](42)
  // val g: G(42) = G(42)
}
