import scala.language.experimental.modularity
import scala.language.future

class C(tracked val x: Int)
class NC(tracked val c: C)
class NNC(tracked val c: D)
class F[A](tracked val a: Int)
class G[A](tracked val a: A)
class NF[A](tracked val f: F[A])
class NG[A](tracked val )

object Test extends App {
  val c: C(42) = C(42)
  val nc: NC(C(42)) = NC(C(42))
  val nc1: NC(c) = NC(c)
  val nnc: NNC(NC(C(42))) = NNC(NC(C(42)))
  val f: F[Int](42) = F[Int](42)
  // val g: G(42) = G(42)
}
