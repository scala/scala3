
object Test2:
  class Foo[+X]
  enum SUB[-S, +T]:
    case Refl[U]() extends SUB[U, U]
  def f[A, B, C](sub : A SUB (B,C)) =
    given Foo[A] = ???
    val x = summon[Foo[A]]
    sub match
      case SUB.Refl() =>
        val c: Foo[(B, C)] = summon[Foo[A]]
        summon[Foo[(B, C)]]
