class ClassThrowsA[T] @throws(classOf[Exception])(someList: List[T]) { // OK, throws applied to the primary constructor
  throw new IllegalArgumentException("Boom!")
}

class ClassThrowsB[T] @throws[Exception]()(someList: List[T]) { // OK, modern version
  throw new IllegalArgumentException("Boom!")
}

@throws[Exception] class ClassNoThrows[T](someList: List[T]) { // NG, throws not applied to primary constructor
  throw new IllegalArgumentException("Boom!")
}

@throws[Exception] object ObjectNoThrows { // NG, throws not applied to primary constructor
  throw new UnsupportedOperationException("Boom!")
}

@throws[Exception] trait TraitNoThrows { // NG, throws not applied to primary constructor
  throw new UnsupportedOperationException("Boom!")
}

@throws[Exception] type AliasNoThrows = Unit // NG, throws not applied to primary constructor

object OuterObj {
  // might be useful to annotate accessors of lazy vals
  @throws[Exception] object InnerObj { // NG, throws not applied to primary constructor
    throw new UnsupportedOperationException("Boom!")
  }
}

trait Various {
  def f[A <: AnyRef](a: A): AnyRef = a: AnyRef @throws[Exception] // there is no check where clearly wrong place

  def g(): Unit = (): @throws[Exception] // no check

  def n(i: Int) = i match { case 42 => 27: @throws[Exception] } // no check
}
