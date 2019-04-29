object G {
    trait Wizzle[L <: Int with Singleton] {
        type Bar[A] = A match {
            case 0 => String
            case L => Int
        }

        def left(fa: String): Bar[0] = fa
        def right(fa: Bar[L]): Int = fa // error

        def center[F[_]](fa: F[0]): F[L]

        def run: String => Int = left andThen center[Bar] andThen right
    }

    class Wozzle extends Wizzle[0] {
        def center[F[_]](fa: F[0]): F[0] = fa
    }

    def main(args: Array[String]): Unit = {
        val coerce: String => Int = (new Wozzle).run
        println(coerce("hello") + 1)
    }
}
