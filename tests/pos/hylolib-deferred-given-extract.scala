//> using options -language:experimental.modularity -source future
package hylotest
import compiletime.deferred

trait Value[Self]

/** A collection of elements accessible by their position. */
trait Collection[Self]:

  /** The type of the elements in the collection. */
  type Element
  given elementIsValue: Value[Element] = compiletime.deferred

class BitArray

given Value[Boolean] {}

given Collection[BitArray] with
  type Element = Boolean
