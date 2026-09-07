package pkg

/**
 *  A trait.
 *
 *  @define t trait
 *
 *  @define tt $t
 *
 */
trait T {
  /** A method on the $t ($tt) */
  def m(): Int

  /** A final method on the $tt */
  final def m2(): Int = m()
}

/**
 * A class that does not redefine its parent trait's Scaladoc defines.
 */
class C1 extends T {
  override def m(): Int = 1
}

/**
 * A class that inherits from a class that does not redefine its parent trait's Scaladoc defines.
 */
final class C2 extends C1 {
  override def m(): Int = 2
}

/**
 * A class that does redefine one of the defines.
 *
 * @define t class
 */
class C3 extends T {
  override def m(): Int = 3
}

/**
 * A class that redefines the other one.
 *
 * @define tt class4
 */
class C4 extends C3 {
  override def m(): Int = 4
}

trait T2 extends T
/** A class that indirectly extends the original one. */
class C5 extends T2 {
  override def m(): Int = 5
}