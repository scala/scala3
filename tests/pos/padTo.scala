object Test {
  abstract class AbstractIterator[A] extends Iterator[A] {
    override def padTo[B >: A](len: Int, elem: B): Iterator[B] = {
      val it = this
      new AbstractIterator[B] {
        private[this] var i = 0

        // This illustrates a tricky situation for joins
        // The RHS of `val b` has type `A | elem.type` where `elem: B`
        // If we widen `A` first in the join we get a RHS of `Any` and a subsequent
        // type error. The right thing to do is to widen `elem.type` to `B` first.
        def next(): B = {
          val b =
            if (it.hasNext) it.next()
            else if (i < len) elem
            else Iterator.empty.next()
          i += 1
          b
        }

        // Same problem, but without singleton types.
        // This one fails to compile in Scala 2.
        def f[C <: B](c: () => C): B = {
          val b =
            if (it.hasNext) it.next()
            else c()
          b
        }

        def hasNext: Boolean = it.hasNext || i < len
      }
    }
  }
}
