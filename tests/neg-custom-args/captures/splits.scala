import caps.Mutable
import caps.any

object M {
  class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y

  def Ref(): Ref^ = new Ref()

  class Conc(val fst: Ref^, val snd: Ref^)

  class Node[A](val fst: A, val snd: A)

  class Abs[A](val fst: A^, val snd: A^)

  def mkNode[A](consume x: A, consume y: A): Node[A] = ???

  def mkConc(consume x: Ref^, consume y: Ref^): Conc^ = Conc(x, y)

  def mkAbs[A <: Any^{}](consume x: A^, consume y: A^): Abs[A]^ = Abs[A](x, y)

  def mkAbs2[A <: Any^{}](consume x: A^, consume y: A^): Abs[A]^ = Abs(x, y)

  class C

  def Test(c: C^): Unit =
    mkNode(c, c)
    var r1 = Ref()
    val r2 = Ref()
    val x = mkNode(r1, r2)  // ok, but can't pull out elements
    val elem1: Ref^ = x.fst
    val elem2: Ref^ = x.snd  // error separation because fst and snd cannot be shown to be separate
    val r3 = Ref()
    val r4 = Ref()
    val y = mkConc(r3, r4) // ok for concrete element types
    val r5: Ref^ = Ref()
    val z = mkConc(r5, r5)  // error separation, as expected
    val r5a = Ref()
    val z1 = mkConc(r5a, r5a)  // error separation, as expected

    val elem3: Ref^ = y.fst
    val elem4: Ref^ = y.snd
    val y1 = mkConc(elem3, elem4) // ok, can split & join for concrete element types

    val r6 = Ref()
    val r7 = Ref()
    val z2 = mkAbs[Ref^{}](r6, r7)

    val elem5: Ref^ = z2.fst
    val elem6: Ref^ = z2.snd
    val z3 = mkAbs(elem5, elem6)   // ok, can spit and join for polymorphic as well.
}