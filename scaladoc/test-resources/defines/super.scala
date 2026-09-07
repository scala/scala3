package pkg

/** Hello */
class Super {
  /** Base */
  def m(): Int = 0
}

/** $super World! */
class Derived extends Super {
  /** Derived from $super. */
  override def m(): Int = 1
}