package dotty.tools
package repl
package terminal

/**
  * The way you configure your terminal behavior; a trivial wrapper around a
  * function, though you should provide a good `.toString` method to make
  * debugging easier. The [[TermInfo]] and [[TermAction]] types are its
  * interface to the terminal.
  *
  * [[Filter]]s are composed sequentially: if a filter returns `None` the next
  * filter is tried, while if a filter returns `Some` that ends the cascade.
  * While your `op` function interacts with the terminal purely through
  * immutable case classes, the Filter itself is free to maintain its own state
  * and mutate it whenever, even when returning `None` to continue the cascade.
  */
trait Filter {
  def op(ti: TermInfo): Option[TermAction]

  /**
    * the `.toString` of this object, except by making it separate we force
    * the implementer to provide something and stop them from accidentally
    * leaving it as the meaningless default.
    */
  def identifier: String

  override def toString = identifier
}

/**
  * Convenience constructors to create [[Filter]] instances in a bunch of
  * different ways
  */
object Filter {

  /**
    * Shorthand to construct a filter in the common case where you're
    * switching on the prefix of the input stream and want to run some
    * transformation on the buffer/cursor
    */
  def simple(prefixes: String*)
            (f: (Vector[Char], Int, TermInfo) => (Vector[Char], Int)): Filter = new Filter {
    def op(ti: TermInfo) = {
      val matchingPrefixOpt =
        prefixes.iterator
                .map{ s => ti.ts.inputs.dropPrefix(s.map(_.toInt)) }
                .collectFirst{case Some(s) => s}
      matchingPrefixOpt.map{ rest =>
        val (buffer1, cursor1) = f(ti.ts.buffer, ti.ts.cursor, ti)
        TermState(
          rest,
          buffer1,
          cursor1
        )

      }
    }
    def identifier = prefixes.mkString(":")
  }
  def action(id: String)
            (prefixes: Seq[String], filter: TermInfo => Boolean = _ => true)
            (action: TermState => TermAction = x => x): Filter = new Filter {
    def op(ti: TermInfo) = {
      prefixes.iterator
              .map{ prefix => ti.ts.inputs.dropPrefix(prefix.map(_.toInt)) }
              .collectFirst { case Some(rest) if filter(ti) => action(ti.ts.copy(inputs = rest)) }
    }
    def identifier: String = id
  }

  def partial(id: String)(f: PartialFunction[TermInfo, TermAction]): Filter =
    new Filter {
      def op(ti: TermInfo) = f.lift(ti)
      def identifier = id
    }

  def wrap(id: String)(f: TermInfo => Option[TermAction]): Filter =
    new Filter {
      def op(ti: TermInfo) = f(ti)
      def identifier = id
    }

  /**
    * Merges multiple [[Filter]]s into one.
    */
  def merge(pfs: Filter*) = new Filter {

    def op(v1: TermInfo) = pfs.iterator.map(_.op(v1)).find(_.isDefined).flatten

    def identifier = pfs.iterator.map(_.identifier).mkString(":")
  }
  val empty = Filter.merge()
}


/**
  * A filter as an abstract class, letting you provide a [[filter]] instead of
  * an `op`, automatically providing a good `.toString` for debugging, and
  * providing a reasonable "place" inside the inheriting class/object to put
  * state or helpers or other logic associated with the filter.
  */
abstract class DelegateFilter(val identifier: String) extends Filter {
  def filter: Filter
  def op(ti: TermInfo) = filter.op(ti)
}
