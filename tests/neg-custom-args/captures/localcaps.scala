class C:
  def x: C^{cap[d]} = ??? // error

  def y: C^{cap[C]} = ???  // ok
  private val z = (x: Int) => (c: C^{cap[z]}) => x // ok

  private val z2 = identity((x: Int) => (c: C^{cap[z2]}) => x) // error
