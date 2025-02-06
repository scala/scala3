import scala.language.experimental.modularity

class Box(tracked val v: Any)
class C(tracked val x: Int)
class NC(tracked val c: C)
class NNC(tracked val c: NC)
class F[A](tracked val a: Int)
class G[A](tracked val a: A)
class NF[A](tracked val f: F[A])

object O:
  val m: Int = 42

  class InnerClass(tracked val x: Int)

object Test extends App {
  val c: C(42) = C(42)
  val nc: NC(C(42)) = NC(C(42))
  val nc1: NC(c) = NC(c)
  val nnc: NNC(NC(C(42))) = NNC(NC(C(42)))
  val f: F[Int](42) = F[Int](42)
  val f2: F[Int](42) = F(42)
  val f3: F(42) = F(42)
  val g: G(42) = G(42)

  val n: Int = 42
  val c2: C(n) = C(n)
  val c3: C(O.m) = C(O.m)

  val box: Box(O.InnerClass(42)) = Box(O.InnerClass(42))
  val box2: Box(O.InnerClass(n)) = Box(O.InnerClass(n))
  val box3: Box(O.InnerClass(O.m)) = Box(O.InnerClass(O.m))
}
