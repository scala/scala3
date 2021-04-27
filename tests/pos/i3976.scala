object Test {
  enum Hoge[F[_]] {
    case A extends Hoge[List]
    case B extends Hoge[[X] =>> String]
  }
  import Hoge.*

  A == A
  A == (B: Hoge[_])
}
