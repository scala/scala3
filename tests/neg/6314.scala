final class X
final class Y

object Test1 {
  trait Test {
    type Type
    // This is testing that both permutations of the types in a &
    // are taken into account by the intersection test
    val i: Bar[Y & Type] = 1 // error
  }

  type Bar[A] = A match {
    case X & Y => String
    case Y => Int
  }
}

object Test2 {
  trait Wizzle[L <: Int with Singleton] {
    type Bar[A] = A match {
      case 0 => String
      case L => Int
    }

    // This is testing that we don't make wrong assumptions about Singleton
    def right(fa: Bar[L]): Int = fa // error
  }

  trait Wazzlo[L <: Int with AnyVal] {
    type Bar[A] = A match {
      case 0 => String
      case L => Int
    }

    // This is testing that we don't make wrong assumptions about AnyVal
    def right(fa: Bar[L]): Int = fa // error
  }

  trait Wuzzlu[L <: String with AnyRef] {
    type Bar[A] = A match {
      case "" => String
      case L => Int
    }

    // This is testing that we don't make wrong assumptions about AnyRef
    def right(fa: Bar[L]): Int = fa // error
  }
}
