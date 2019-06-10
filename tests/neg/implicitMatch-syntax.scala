object Test {
  import collection.immutable.TreeSet
  import collection.immutable.HashSet

  inline def f1[T] = delegate delegate match { // error: illegal modifier // error: ';' expected, but 'match' found // error: Declaration of method f1 not allowed here
    case ord: Ordered[T] => new TreeSet[T]
    case _ => new HashSet[T]

  }

  inline def f2[T] = delegate erased match { // error: illegal modifier // error: illegal modifier // error: Declaration of method f1 not allowed here
    case ord: Ordered[T] => new TreeSet[T]
    case _ => new HashSet[T]
  }

  inline def f3[T] = erased delegate match { // error: illegal modifier
    case ord: Ordered[T] => new TreeSet[T]
    case _ => new HashSet[T]
  }

  inline def f4() = delegate match {
    case Nil => ???     // error: not a legal pattern
    case x :: xs => ??? // error: not a legal pattern
  }

  inline def f5[T] = locally { delegate match { // Ok
    case _ => new HashSet[T]
  }}

  def f6[T] = delegate match { // error: delegate match cannot be used here
    case _ => new HashSet[T]
  }
}