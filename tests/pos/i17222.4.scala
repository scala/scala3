import scala.compiletime.*

trait Reader[-In, Out]

trait A:
  type T
  type F[X]
  type Q = F[T]

given [X]: Reader[A { type Q = X }, X] with {}

case class Box[T](x: T)

/** compiletime.summonAll, but with one case */
inline def summonOne[T]: T =
  val res =
    inline erasedValue[T] match
      case _: Box[t] => summonInline[t]
    end match
  Box(res).asInstanceOf[T]
end summonOne


@main def main =


  trait B[X] extends A:
    type T = X

  trait C extends A:
    type F[X] = X


  val bc = new B[Int] with C

  summonOne[Box[Reader[bc.type, Int]]] // errors


  val bc2: A { type Q = Int } = new B[Int] with C

  summonOne[Box[Reader[bc2.type, Int]]] // works


  object BC extends B[Int] with C

  summonOne[Box[Reader[BC.type, Int]]] // works


  val a = new A:
    type T = Int
    type F[X] = X

  summonOne[Box[Reader[a.type, Int]]] // works


  val b = new B[Int]:
    type F[X] = X

  summonOne[Box[Reader[b.type, Int]]] // works


  val ac = new A with C:
    type T = Int

  summonOne[Box[Reader[ac.type, Int]]] // works


  trait D[X] extends B[X] with C
  val d = new D[Int] {}

  summonOne[Box[Reader[d.type, Int]]] // works
