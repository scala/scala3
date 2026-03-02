package colltestMut
import caps.*
import collection.*, mutable.*, immutable.*

object collection:
  trait Iterable[A]:
    this: Iterable[A]^ =>
    type C[X] <: Iterable[X]^
    def map[B](f: A => B): C[B]^{f} = ???

  trait View[A] extends Iterable[A]:
    this: View[A]^ =>
    type C[X] = View[X]^{this}

  trait Seq[A] extends Iterable[A]:
    this: Seq[A]^ =>
    override def map[B](f: A => B): C[B] = ???

object immutable:
  import collection.*
  trait Seq[A] extends Iterable[A], Pure:
    type C[X] <: Seq[X]

  class List[A] extends Seq[A]:
    type C[X] = List[X]


object mutable:
  import collection.*
  trait Seq[A] extends collection.Seq[A]:
    this: Seq[A]^ =>
    type C[X] <: Seq[X]^

  trait Buffer[A] extends Seq[A], Mutable:
    this: Buffer[A]^ =>
    type C[X] = Buffer[X]^

class IO extends SharedCapability
class Ref extends Mutable

object Test:
  def test(io: IO, ref: Ref, f: Int => Int) =
    val xs1: List[Int] = ???
    val ys1 = xs1.map(f)
    val xs2: Buffer[Int]^ = ???
    val ys2 = xs2.map(f)
    val xs3: Buffer[Int]^ = ???
    val zs3 = freeze(xs3)
    val xs4: Buffer[Int]^ = ???
    val vs4: View[Int]^{xs4} = ???
    val ys4 = vs4.map(f)
