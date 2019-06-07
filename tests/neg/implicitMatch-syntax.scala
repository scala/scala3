object Test {
  import collection.immutable.TreeSet
  import collection.immutable.HashSet

  inline def f1[T] = implied implied match { // error: illegal modifier // error: ';' expected, but 'match' found // error: Declaration of method f1 not allowed here
    case ord: Ordered[T] => new TreeSet[T]
    case _ => new HashSet[T]

  }

  inline def f2[T] = implied erased match { // error: illegal modifier // error: illegal modifier // error: Declaration of method f1 not allowed here
    case ord: Ordered[T] => new TreeSet[T]
    case _ => new HashSet[T]
  }

  inline def f3[T] = erased implied match { // error: illegal modifier
    case ord: Ordered[T] => new TreeSet[T]
    case _ => new HashSet[T]
  }

  inline def f4() = implied match {
    case Nil => ???     // error: not a legal pattern
    case x :: xs => ??? // error: not a legal pattern
  }

  inline def f5[T] = locally { implied match { // Ok
    case _ => new HashSet[T]
  }}

  def f6[T] = implied match { // error: implied match cannot be used here
    case _ => new HashSet[T]
  }
}