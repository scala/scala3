object Test1 {
  // X, Y and Z are unrelated, Y is provably disjoint from Z, but X is not provably disjoint with either
  trait X
  class Y
  class Z

  trait Test {
    type Type
    // This is testing that both permutations of the types in a &
    // are taken into account by the provablyDisjoint test
    val i: Bar[Y & Type] = 1 // ok, disjoint from X & Z because Y and Z are disjoint
  }

  type Bar[A] = A match {
    case X & Z => String
    case Y => Int
  }
}

object Test1Bis {
  final class X
  final class Y

  trait Test {
    type Type
    // This is testing that both permutations of the types in a |
    // are taken into account by the provablyDisjoint test
    val i: Bar[Y | Type] = 1 // error
  }

  type Bar[A] = A match {
    case X & Y => String
    case Any => Int
  }
}

object Test2 {
  trait Wizzle[L <: Int & Singleton] {
    type Bar[A] = A match {
      case 0 => String
      case L => Int
    }

    // This is testing that we don't make wrong assumptions about Singleton
    def right(fa: Bar[L]): Int = fa // error
  }

  trait Wazzlo[L <: Int & AnyVal] {
    type Bar[A] = A match {
      case 0 => String
      case L => Int
    }

    // This is testing that we don't make wrong assumptions about AnyVal
    def right(fa: Bar[L]): Int = fa // error
  }

  trait Wuzzlu[L <: String & AnyRef] {
    type Bar[A] = A match {
      case "" => String
      case L => Int
    }

    // This is testing that we don't make wrong assumptions about AnyRef
    def right(fa: Bar[L]): Int = fa // error
  }
}
