object G {
    trait Wizzle {
        type X <: Int with Singleton
        type Y <: Int with Singleton

        type Bar[A] = A match {
            case X => String
            case Y => Int
        }

        def left(fa: String): Bar[X] = fa
        def center[F[_]](fa: F[X]): F[Y]
        def right(fa: Bar[Y]): Int = fa // error

        def run: String => Int = left andThen center[Bar] andThen right
    }

    class Wozzle extends Wizzle {
        type X = 0
        type Y = 0
        def center[F[_]](fa: F[X]): F[Y] = fa
    }

    def main(args: Array[String]): Unit = {
        val coerce: String => Int = (new Wozzle).run
        println(coerce("hello") + 1)
    }
}
