object Test {
  type T[X] = X match {
    case String => Int
    case Int => String
  }

  trait Nat {
    def toInt: Int = ???
  }

  case object Z extends Nat
  case class S[N <: Nat] extends Nat
  type Z = Z.type

  type Len[X] = X match {
    case Unit => Z
    case (x, xs) => S[Len[xs]]
  }
}