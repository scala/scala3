package test

/** A class defining symbols and types of standard definitions */
class Definitions {

  trait LazyType { def complete(): Unit }

  def f(vcs: List[Int]): Unit = {
    val completer = new LazyType  {
      def complete(): Unit =
        for (i <- 0 until vcs.length if vcs(i) != 0)
          f(vcs.updated(i, 0))
    }
  }
}
