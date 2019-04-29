object G {
  type Void <: Nothing
  trait Wizzle {
    type Razzle[+X >: Void]
    type X = 0
    type Y = 1

    type Bar[A] = A match {
      case Razzle[X] => String
      case Razzle[Y] => Int
    }

    def left(fa: String): Bar[Razzle[X]] = fa
    def center[F[_]](fa: F[Razzle[X]]): F[Razzle[Y]]
    def right(fa: Bar[Razzle[Y]]): Int = fa // error

    def run: String => Int = left andThen center[Bar] andThen right
  }

  class Wozzle extends Wizzle {
    type Razzle[+X >: Void] = Int
    def center[F[_]](fa: F[Razzle[X]]): F[Razzle[Y]] = fa
  }

  def main(args: Array[String]): Unit = {
    val coerce: String => Int = (new Wozzle).run
    println(coerce("hello") + 1)
  }
}
