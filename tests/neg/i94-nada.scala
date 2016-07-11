trait Test1 {
  trait Monad[MX] {
    def x: MX
  }
  sealed abstract class Either[A,B]
  case class Left[A,B](x: A) extends Either[A,B] with Monad[A]
  case class Right[A,B](x: B) extends Either[A,B] with Monad[B]
  def flatMap[FX,FY,M[FMX]<:Monad[FMX]](m: M[FX], f: FX => M[FY]): M[FY] = f(m.x)
  println(flatMap(Left(1), {x: Int => Left(x)})) // error: Left does not conform to [X] -> Monad[X]

}
