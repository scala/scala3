object Test {
  import collection.immutable.TreeSet
  import collection.immutable.HashSet

  transparent def f1[T] = implicit implicit match { // error: repeated modifier // error: illegal modifier
    case ord: Ordered[T] => new TreeSet[T] // error: no implicit
    case _ => new HashSet[T]

  }

  transparent def f2[T] = implicit erased match { // error: illegal modifier
    case ord: Ordered[T] => new TreeSet[T] // error: no implicit
    case _ => new HashSet[T]
  }

  transparent def f3[T] = erased implicit match { // error: illegal modifier
    case ord: Ordered[T] => new TreeSet[T] // error: no implicit
    case _ => new HashSet[T]
  }

  transparent def f4() = implicit match {
    case Nil => ???     // error: not a legal pattern
    case x :: xs => ??? // error: not a legal pattern
  }

}