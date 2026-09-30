package pkg

/**
 * @define x xxx
 * @define y yyy
 */
trait First {
  /** m: $x */
  def m(): Int = 0
}

trait Second { self: First =>
  /** m2: $x $y */
  def m2(): Int = m()
}

/**
 * @define y overridden
 */
class C extends Second, First {

}